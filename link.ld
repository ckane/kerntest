/*
 * Custom linker script for the kernel that is an artefact of the cargo build.
 */

/* Symbol comes from start.S */
ENTRY(_start)

SECTIONS {

    . = 0xffffe00000000000;
    .text . :
    {
        *(.text .text.*)
    }

    . = ALIGN(4K);
    .rodata :
    {
      *(.rodata .rodata.*)
    }

    . = ALIGN(4K);
    .data :
    {
      *(.data .data.*)
    }

    . = ALIGN(4K);
    .bss :
    {
      *(COMMON)
      *(.bss .bss.*)
    }


}
