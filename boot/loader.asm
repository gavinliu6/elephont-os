;/********************************************************************************
;   内核加载器 Loader
;
;   采用 Intel 汇编语言格式编写，使用 NASM 编译器编译
; 
;   本程序由 MBR 程序加载至物理起始地址为 0x900 处的内存中
;
;   MBR 程序通过最后一条指令 `jmp LOADER_BASE_ADDR` 移交控制权给本程序
;   CS = 0x0000
;
;   编译命令
;       nasm -I include/ -o loader.bin loader.asm
;   编译完成后，借助 `dd` 命令行工具将 loader.bin 写入到 Bochs 虚拟设备的第 2 个扇区中
;       dd if=path/loader.bin of=path/*.img bs=512 count=1 seek=2 conv=notrunc
;
;********************************************************************************/

%include "boot.inc"
section loader vstart=LOADER_BASE_ADDR ; 告知编译器，把本程序的起始地址编译为 0x900

    ;--------------------------------------------------
    ; 构建全局描述符表 GDT 及其内部的描述符
    ;--------------------------------------------------
    
    ; GDT 第 0 个表项：null 描述符
    GDT_BASE: dd 0x00000000
              dd 0x00000000
    
    ; GDT 第 1 个表项：代码段描述符
    CODE_DESC: dd 0x0000FFFF
               dd DESC_CODE_HIGH4

    ; GDT 第 2 个表项：数据段和栈段描述符
    DATA_STACK_DESC: dd 0x0000FFFF
                     dd DESC_DATA_HIGH4

    ; GDT 第 3 个表项：显存段描述符
    VIDEO_DESC: dd 0x80000007
                dd DESC_VIDEO_HIGH4

    GDT_SIZE  equ $ - GDT_BASE
    GDT_LIMIT equ GDT_SIZE - 1

    times 60 dq 0 ; 此处预留 60 个描述符的空位

    SELECTOR_CODE  equ (0x0001<<3) + TI_GDT + RPL0 ; 代码段选择子
    SELECTOR_DATA  equ (0x0002<<3) + TI_GDT + RPL0 ; 数据段选择子
    SELECTOR_VIDEO equ (0x0003<<3) + TI_GDT + RPL0 ; 显存段选择子

    ;-----------------------------------------------------------
    ; 以字节为单位保存内存容量
    ;-----------------------------------------------------------
    ; 变量 total_mem_bytes 相对于 loader.bin 文件起始位置偏移 0x200
    ; 0x900 => loader.bin
    ; 0x900 + 200 = 0xb00 => total_mem_bytes
    ;-----------------------------------------------------------
    total_mem_bytes dd 0

    ;-----------------------------------------------------------
    ; 定义 GDT 指针，前 2 字节是 GDT 的界限，后 4 字节是 GDT 起始地址
    ;-----------------------------------------------------------
    gdt_ptr  dw  GDT_LIMIT 
             dd  GDT_BASE

    ;-----------------------------------------------------------
    ; 定义地址范围描述符结构 ARDS 缓冲区
    ;-----------------------------------------------------------
    ; 这里之所以分配 244 个字节是为了对齐
    ; total_mem_bytes + gdt_ptr + ards_buf + ards_nr2
    ; 4               + 6       + 244      + 2
    ; 共 256 字节
    ; 所以，下面的 loader_start 在文件内偏移 0x200 + 0x100 = 0x300
    ;-----------------------------------------------------------
    ards_buf times 244 db 0
    ards_nr  dw 0

loader_start:
   
    ;-----------------------------------------------------------
    ; 借助 BIOS 中断获取内存信息
    ;-----------------------------------------------------------

    xor ebx, ebx
    mov edx, 0x534d4150
    mov di, ards_buf
.e820_mem_get_loop:
    mov eax, 0x0000e820
    mov ecx, 20
    int 0x15
    jc .e820_failed_so_try_e801
    add di, cx
    inc word [ards_nr]
    cmp ebx, 0
    jnz .e820_mem_get_loop

    mov cx, [ards_nr]
    mov ebx, ards_buf 
    xor edx, edx
.find_max_mem_area:
    mov eax, [ebx]
    add eax, [ebx+8]
    add ebx, 20
    cmp edx, eax
    jge .next_ards
    mov edx, eax
.next_ards:
    loop .find_max_mem_area
    jmp .mem_get_ok

.e820_failed_so_try_e801:
    mov ax, 0xe801
    int 0x15
    jc .e801_failed_so_try88

    mov cx, 0x400
    mul cx 
    shl edx, 16
    and eax, 0x0000FFFF
    or edx, eax
    add edx, 0x100000
    mov esi, edx

    xor eax, eax
    mov ax, bx		
    mov ecx, 0x10000
    mul ecx
    add esi, eax
    mov edx, esi
    jmp .mem_get_ok

.e801_failed_so_try88:
    mov ah, 0x88
    int 0x15
    jc .error_hlt
    and eax, 0x0000FFFF
   
    mov cx, 0x400
    mul cx
    shl edx, 16
    or edx, eax
    add edx, 0x100000

.mem_get_ok:
    mov [total_mem_bytes], edx ; 将内存换为 byte 单位后存入 total_mem_bytes 处

    ;-----------------------------------------------------------
    ; 进入保护模式第 1 步：打开 A20 地址线
    ;-----------------------------------------------------------
    in al,0x92
    or al,0000_0010B
    out 0x92,al

    ;-----------------------------------------------------------
    ; 进入保护模式第 2 步：为 GDTR 寄存器赋值
    ;-----------------------------------------------------------
    lgdt [gdt_ptr]

    ;-----------------------------------------------------------
    ; 进入保护模式第 3 步：置位 CR0 控制寄存器的第 0 位，以开启保护模式
    ;-----------------------------------------------------------
    mov eax, cr0
    or eax, 0x00000001
    mov cr0, eax

    jmp dword SELECTOR_CODE:p_mode_start ; 刷新流水线

.error_hlt:
    hlt

[bits 32] ; 指示编译器生成的代码将运行在 32 位宽的处理器上
p_mode_start:
    mov ax, SELECTOR_DATA
    mov ds, ax
    mov es, ax
    mov ss, ax
    mov esp, LOADER_STACK_TOP
    mov ax, SELECTOR_VIDEO
    mov gs, ax

    ;-----------------------------------------------------------
    ; 加载内核进内存缓冲区
    ;-----------------------------------------------------------
    mov eax, KERNEL_START_SECTOR  ; EAX = 0x9 = kernel.bin 在硬盘上的 LBA 地址
    mov ebx, KERNEL_BIN_BASE_ADDR ; EBX = 0x70000 = kernel.bin 在内存中的位置
    mov ecx, 200                  ; ECX = 200 = 待读入的扇区数

    call rd_disk_m_32

    ;-----------------------------------------------------------
    ; 开启分页机制
    ;-----------------------------------------------------------

    ; 创建页目录表 PDT 及页表 PT，并初始化页内存位图
    call setup_page

    sgdt [gdt_ptr]

    ; 将 gdt 描述符中视频段描述符中的段基址 + 0xc0000000
    mov ebx, [gdt_ptr + 2]  
    or dword [ebx + 0x18 + 4], 0xc0000000

    ; 将 gdt 的基址加上 0xc0000000 使其成为内核所在的高地址
    add dword [gdt_ptr + 2], 0xc0000000

    ; 将栈指针同样映射到内核地址
    add esp, 0xc0000000

    ; CR3 = 内核顶层页目录地址
    mov eax, PAGE_DIR_TABLE_POS
    mov cr3, eax

    ; 打开 CR0 控制寄存器的第 31 位的 PG 位，以开启分页
    mov eax, cr0
    or eax, 0x80000000
    mov cr0, eax

    lgdt [gdt_ptr]

    jmp SELECTOR_CODE:enter_kernel
enter_kernel:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    call kernel_init
    mov esp, 0xc009f000

    ;-----------------------------------------------------------
    ; 进入内核
    ;-----------------------------------------------------------
    jmp KERNEL_ENTRY_POINT ; 0xc0001500

; 将 kernel.bin 中的 segment 拷贝到编译的地址
kernel_init:
    xor eax, eax
    xor ebx, ebx
    xor ecx, ecx
    xor edx, edx

    mov dx, [KERNEL_BIN_BASE_ADDR + 42]	 ; dx = e_phentsize = 程序头表成员的大小 = 一个 VMA 的字节数
    mov ebx, [KERNEL_BIN_BASE_ADDR + 28] ; ebx = e_phoff = 程序头表在 ELF 文件中的偏移
                                         ; 其实该值是 0x34,不过还是谨慎一点，这里来读取实际值
    add ebx, KERNEL_BIN_BASE_ADDR        ; ebx = 程序头的物理基地址
    mov cx, [KERNEL_BIN_BASE_ADDR + 44]  ; cx = e_phnum = 程序头表的数量 = VMA 的数量
.each_segment:
    cmp byte [ebx + 0], PT_NULL          ; 若 p_type 等于 PT_NULL，说明此 VMA 未使用
    je .PTNULL

    push dword [ebx + 16]                ; 压入 mem_cpy 函数的第 3 个参数 size
                                         ; 一个程序头内偏移 16 字节的地方是 p_filesz，即，该 segment 在 ELF 文件中所占空间的长度，亦即，
    mov eax, [ebx + 4]                   ; eax = p_offset = 该 segment 在 ELF 文件中的偏移
    add eax, KERNEL_BIN_BASE_ADDR        ; eax = 该 segment 的物理基地址
    push eax                             ; 压入 mem_cpy 函数的第 2 个参数 src
    push dword [ebx + 8]                 ; 压入 mem_cpy 函数的第 1 个参数 dst
                                         ; 一个程序头内偏移 8 字节的地方是 p_vaddr，即，该 segment 的第一个字节在进程虚拟地址空间的起始位置
    call mem_cpy
    add esp, 12
.PTNULL:
    add ebx, edx				         ; edx = 程序头表的大小，ebx => 下一个 program header 
    loop .each_segment
    ret

;-----------------------------------------------------------
; 将 src 指向的地址空间处的连续 size 个字节拷贝到 dst 指向的地址空间
;-----------------------------------------------------------
mem_cpy:
    cld                 ; 清除方向标志位 DF
    push ebp
    mov ebp, esp
    push ecx
    mov edi, [ebp + 8]  ; dst
    mov esi, [ebp + 12] ; src
    mov ecx, [ebp + 16] ; size
    rep movsb

    pop ecx
    pop ebp
    ret

;-----------------------------------------------------------
; 创建页目录及页表
;-----------------------------------------------------------
.setup_page:
    mov ecx, 4096
    mov esi, 0

; 先把页目录占用的空间逐字节清 0
.clear_page_dir:
    mov byte [PAGE_DIR_TABLE_POS + esi], 0
    inc esi
    loop .clear_page_dir

; 开始创建页目录项 PDE
.create_pde:
    mov eax, PAGE_DIR_TABLE_POS           ; EAX = 0x100000 = 顶层页目录表 PDT 的物理基地址，即，出了低端 1MB 物理空间的第一个字节
    add eax, 0x1000                       ; EAX = 0x101000
    mov ebx, eax                          ; EBX = 0x101000 = 第 0 个页表的物理基地址

    or eax, PG_US_U | PG_RW_W | PG_P
    mov [PAGE_DIR_TABLE_POS + 0x0], eax   ; 页目录项第 0 项 <—— 0x0~0x3FFFFF 共 4MB，这是为了保证 loader 程序本身能够正确运行
    mov [PAGE_DIR_TABLE_POS + 0xc00], eax ; 页目录项第 768 项 <—— 0xC0000000~0xC03FFFFF 共 4MB
    sub eax, 0x1000
    mov [PAGE_DIR_TABLE_POS + 4092], eax  ; 最后一个页目录项 PDE 指向页目录表 PDT 自己的地址

; 下面创建页表项 PTE
    mov ecx, 256                          ; 这 256 个页表项映射低端 1MB 的物理内存
    mov esi, 0
    mov edx, PG_US_U | PG_RW_W | PG_P
.create_pte:
    mov [ebx+esi*4], edx                  ; 此时的 ebx 已经在上面通过 eax 赋值为 0x101000，即，第一个页表的地址 
    add edx, 4096
    inc esi
    loop .create_pte

; 创建内核第 769~1022 项的 PDE
    mov eax, PAGE_DIR_TABLE_POS
    add eax, 0x2000
    or eax, PG_US_U | PG_RW_W | PG_P
    mov ebx, PAGE_DIR_TABLE_POS
    mov ecx, 254
    mov esi, 769
.create_kernel_pde:
    mov [ebx+esi*4], eax
    inc esi
    add eax, 0x1000
    loop .create_kernel_pde
    ret

; 读取硬盘 n 个扇区
rd_disk_m_32:
    mov esi, eax
    mov di, cx
    mov dx, 0x1f2
    mov al, cl
    out dx, al

    mov eax, esi

    mov dx, 0x1f3                       
    out dx, al

    mov cl, 8
    shr eax, cl
    mov dx, 0x1f4
    out dx, al
    
    shr eax, cl
    mov dx, 0x1f5
    out dx, al

    shr eax, cl
    and al, 0x0f
    or al, 0xe0
    mov dx, 0x1f6
    out dx, al

    mov dx, 0x1f7
    mov al, 0x20                        
    out dx, al

.not_ready:
    nop
    in al, dx
    and al, 0x88
    cmp al, 0x08
    jnz .not_ready

    mov ax, di

    mov dx, 256
    mul dx
    mov cx, ax	   
    mov dx, 0x1f0
  .go_on_read:
    in ax, dx		
    mov [ebx], ax
    add ebx, 2

    loop .go_on_read
    ret
