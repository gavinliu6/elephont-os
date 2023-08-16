;/********************************************************************************
;   主引导程序 MBR
;
;   采用 Intel 汇编语言格式编写，使用 NASM 编译器编译
; 
;   本程序由 BIOS 固件程序加载至物理起始地址为 0x7c00 处的内存中，共占 512 个字节
;   其物理地址空间是：0x7c00 ~ 0x7e00
;
;   BIOS 通过最后一条指令 `jmp 0:0x7c00` 移交控制权给本程序
;   CS = 0x0000
;
;   编译命令
;       nasm -I include/ -o mbr.bin mbr.asm
;   编译完成后，借助 `dd` 命令行工具将 mbr.bin 写入到 Bochs 虚拟设备的引导扇区中
;       dd if=path/mbr.bin of=path/*.img bs=512 count=1 conv=notrunc
;
;********************************************************************************/

%include "boot.inc"

SECTION MBR vstart=0x7c00 ; 告知编译器，把本程序的起始地址编译为 0x7c00

    mov ax, cs
    mov ds, ax
    mov es, ax
    mov ss, ax
    mov fs, ax
    mov sp, 0x7c00
    mov ax, 0xb800 ; 0xb8000 ~ 0xbffff 这 32KB 的内存用于映射文本模式的显存空间
    mov gs, ax

    ;--------------------------------------------------
    ; 清屏
    ;--------------------------------------------------
    ; INT 0x10, 0x06    功能描述：上卷指定范围的窗口
    ;--------------------------------------------------
    ; 输入参数
    ; AH = 0x06 = 主功能号
    ; AL = 上卷的行数，若为 0 表示全部
    ; BH = 上卷行属性
    ; (CL,CH) = 窗口左上角的 (X,Y) 位置
    ; (DL,DH) = 窗口右下角的 (X,Y) 位置
    ;--------------------------------------------------
    ; 无返回值
    ;--------------------------------------------------
    mov ax, 0x600  ; AH = 0x06 | AL = 0
    mov bx, 0x700
    mov cx, 0      ; 左上角 (0, 0)
    mov dx, 0x184f ; 右下角 (80, 25)
                   ; VGA 文本模式中，一行只能容纳 80 个字符，共 25 行
    int 0x10

    ;--------------------------------------------------
    ; 操作显存，以输出绿色背景，红色字体的跳动字符串 "1 MBR"
    ;--------------------------------------------------
    mov byte [gs:0x00], '1'
    mov byte [gs:0x01], 0xA4 ; 红色跳动字符，绿色背景

    mov byte [gs:0x02], ' '
    mov byte [gs:0x03],0xA4

    mov byte [gs:0x04], 'M'
    mov byte [gs:0x05], 0xA4

    mov byte [gs:0x06], 'B'
    mov byte [gs:0x07], 0xA4

    mov byte [gs:0x08], 'R'
    mov byte [gs:0x09], 0xA4

    ;--------------------------------------------------
    ; 加载 loader.bin 进内存
    ;--------------------------------------------------
    mov eax, LOADER_START_SECTOR ; EAX = 0x2 = loader 在硬盘上的 LBA 地址
    mov bx, LOADER_BASE_ADDR     ; BX = 0x900 = loader 在内存中的位置
    mov cx, 4                    ; CX = 4 = 待读入的扇区数
    call rd_disk_m_16

    jmp LOADER_BASE_ADDR + 0x300

;--------------------------------------------------
; 读取硬盘 n 个扇区
;--------------------------------------------------
; 输入参数
; EAX = 待读入扇区的起始地址，初始值为 0x2
; CX = 要读取的扇区数
; BX = 从硬盘读入数据后的存放位置，初始值为 0x900
;--------------------------------------------------
rd_disk_m_16:
    mov esi, eax ; 备份 EAX
    mov di, cx   ; DI = CX = 1

; 读写硬盘第 1 步：设置要读取的扇区数
    mov dx, 0x1f2
    mov al, cl
    out dx, al

    mov eax, esi ; 恢复 EAX 的初始值

; 读写硬盘第 2 步：将 LBA 地址存入 0x1f3 ~ 0x1f6

    ; LBA 地址 7~0 位写入端口 0x1f3
    mov dx, 0x1f3
    out dx, al

    ; LBA 地址 15~8 位写入端口 0x1f4
    mov cl, 8
    shr eax, cl
    mov dx, 0x1f4
    out dx, al

    ; LBA 地址 23~16 位写入端口 0x1f5
    shr eax, cl
    mov dx, 0x1f5
    out dx, al

    shr eax, cl
    and al, 0x0f
    or al, 0xe0
    mov dx, 0x1f6
    out dx, al

; 读写硬盘第 3 步：向 0x1f7 端口写入读命令
    mov dx, 0x1f7
    mov al, 0x20
    out dx, al

; 读写硬盘第 4 步：检测硬盘状态
    .not_ready: ; .not_ready 只是一个标号
        ; 同一端口，写时表示写入命令字，读时表示读入硬盘状态
        nop
        in al, dx
        and al, 0x88 ; 第 4 位为 1 表示硬盘控制器已准备好数据传输
                     ; 第 7 位为 1 表示硬盘忙
        cmp al, 0x08   
        jnz .not_ready ; 若未准备好，继续等

; 读写硬盘第 5 步：从 0x1f0 端口读数据
        mov ax, di
        mov dx, 256
        mul dx        ; 被乘数 AX * 乘数 DX = 执行 in 指令的次数，其中，DX = 积的高 16 位 | AX = 积的低 16 位
        mov cx, ax    ; 我们事先知晓乘积不是很大，所以这里只将乘积的低 16 位保存了下来，以作为循环读取的次数
        mov dx, 0x1f0
    
    .go_on_read:
        in ax, dx
        mov [bx], ax
        add bx, 2
        loop .go_on_read ; loop 指令使用 CX 寄存器的值作为循环计数器
        ret

    times 510-($-$$) db 0
    db 0x55, 0xaa
