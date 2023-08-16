; --------------------------- loader & kernel ---------------------------

LOADER_BASE_ADDR    equ 0x900            ; loader 的物理基地址
LOADER_STACK_TOP    equ LOADER_BASE_ADDR ; loader 在保护模式下的栈基地址
LOADER_START_SECTOR equ 0x2              ; loader 位于虚拟设备的第 2 个扇区（以 LBA 方式编址）

KERNEL_BIN_BASE_ADDR equ 0x70000    ; kernel 被加载到内存时的物理基地址
KERNEL_START_SECTOR  equ 0x9        ; kernel 位于虚拟设备的第 9 个扇区（以 LBA 方式编址）
KERNEL_ENTRY_POINT   equ 0xc0001500 ; kernel 的起始线性地址 = 0x900 + 2000 字节 = 0x10d0，取整 0x1500

; ------------------------------- 页表配置 -------------------------------
PAGE_DIR_TABLE_POS equ 0x100000 ; 页目录表 PDT 的物理基地址

; ---------------------------- GDT 描述符属性 ----------------------------
DESC_G_4K         equ 1_00000000000000000000000b
DESC_D_32         equ 1_0000000000000000000000b
DESC_L            equ 0_000000000000000000000b
DESC_AVL          equ 0_00000000000000000000b
DESC_LIMIT_CODE2  equ 1111_0000000000000000b
DESC_LIMIT_DATA2  equ DESC_LIMIT_CODE2
DESC_LIMIT_VIDEO2 equ 0000_000000000000000b
DESC_P            equ 1_000000000000000b
DESC_DPL_0        equ 00_0000000000000b
DESC_DPL_1        equ 01_0000000000000b
DESC_DPL_2        equ 10_0000000000000b
DESC_DPL_3        equ 11_0000000000000b
DESC_S_CODE       equ 1_000000000000b
DESC_S_DATA       equ DESC_S_CODE
DESC_S_sys        equ 0_000000000000b
DESC_TYPE_CODE    equ 1000_00000000b
DESC_TYPE_DATA    equ 0010_00000000b

DESC_CODE_HIGH4  equ (0x00 << 24) + DESC_G_4K + DESC_D_32 + DESC_L + DESC_AVL + DESC_LIMIT_CODE2 + DESC_P + DESC_DPL_0 + DESC_S_CODE + DESC_TYPE_CODE + 0x00
DESC_DATA_HIGH4  equ (0x00 << 24) + DESC_G_4K + DESC_D_32 + DESC_L + DESC_AVL + DESC_LIMIT_DATA2 + DESC_P + DESC_DPL_0 + DESC_S_DATA + DESC_TYPE_DATA + 0x00
DESC_VIDEO_HIGH4 equ (0x00 << 24) + DESC_G_4K + DESC_D_32 + DESC_L + DESC_AVL + DESC_LIMIT_VIDEO2 + DESC_P + DESC_DPL_0 + DESC_S_DATA + DESC_TYPE_DATA + 0x0b

; ----------------------------- 选择子属性 -----------------------------
RPL0   equ 00b
RPL1   equ 01b
RPL2   equ 10b
RPL3   equ 11b
TI_GDT equ 000b
TI_LDT equ 100b

; ---------------------------- 页表相关属性 ----------------------------
PG_P     equ 1b
PG_RW_R  equ 00b
PG_RW_W  equ 10b
PG_US_S  equ 000b
PG_US_U  equ 100b 

; ------------------------- program type 定义 -------------------------
PT_NULL equ 0
