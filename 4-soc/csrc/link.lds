/* RISC-V Linker Script for MyCPU Pipeline Implementation
 *
 * This linker script defines the memory layout and section placement for
 * programs running on the MyCPU pipelined RISC-V processor. It is designed
 * for bare-metal execution with GP-relative addressing optimization.
 *
 * Memory Layout:
 *   0x00000000 - 0x00000FFF : Reserved (trap vectors, boot ROM, etc.)
 *   0x00001000 - 0x000FFFFF : Code and data sections
 *   0x00100000 - ...        : BSS section (zero-initialized data)
 *
 * GP (Global Pointer) Optimization:
 *   - GP register (x3) points to __global_pointer$ (middle of .sdata)
 *   - Enables single-instruction access to small data within ±2KB range
 *   - Small data (.sdata) and small BSS (.sbss) use GP-relative addressing
 *
 * ABI Compliance:
 *   - Follows RISC-V ELF psABI specification
 *   - GP initialized to __global_pointer$ in startup code
 *   - Entry point: _start (in .text.init section)
 */

OUTPUT_ARCH("riscv")

/* Entry point for program execution */
ENTRY(_start)

SECTIONS
{
    /* Start address: 4KB offset avoids reserved low memory */
    . = 0x00001000;

    /* .text section: Executable code
     *
     * Section order:
     *   1. .text.init    - Startup code (_start entry point)
     *   2. .text.startup - Early initialization code
     *   3. .text         - Main program code
     *
     * This ordering ensures the processor begins execution at _start.
     */
    .text : {
        *(.text.init)
        *(.text.startup)
        *(.text)
    }

    /* .data section: Initialized data and read-only data
     *
     * Page-aligned (4KB) for potential memory protection.
     * Contains:
     *   - Regular initialized variables (.data*)
     *   - Read-only data (.rodata*)
     *   - Small read-only data (.srodata*)
     *
     * Note: .rodata placed here because MyCPU has no memory protection.
     */
    .data ALIGN(0x1000) : {
        *(.data*)
        *(.rodata*)
        *(.srodata*)
    }

    /* .sdata section: Small data for GP-relative access
     *
     * GP Optimization:
     *   - __global_pointer$ placed at .sdata + 0x800 (2KB offset)
     *   - Allows ±2KB addressing range from GP
     *   - Single-instruction access: lw/sw with GP-relative offset
     *
     * The startup code must initialize GP:
     *   la gp, __global_pointer$
     */
    .sdata : {
        __sdata_start = .;
        PROVIDE(__global_pointer$ = . + 0x800);
        *(.sdata*)
    }

    /* .sbss section: Small zero-initialized data for GP-relative access
     *
     * Combined with .sdata for GP-relative addressing optimization.
     * Startup code must zero this section before main().
     */
    .sbss : {
        __sbss_start = .;
        *(.sbss*)
        *(.scommon)
        . = ALIGN(4);
        __sbss_end = .;
        __sdata_end = .;
    }

    /* BSS section placed at 1MB boundary
     *
     * Separation strategy:
     *   - Small frequently-accessed data (.sdata/.sbss) in low memory
     *   - Large zero-initialized arrays (.bss) at higher address
     *   - Reduces cache conflicts between code/data and large buffers
     */
    . = 0x00100000;

    /* .bss section: Large zero-initialized data
     *
     * Page-aligned (4KB) and placed at 1MB boundary.
     * Startup code must zero from __bss_start to __bss_end.
     * Does not consume space in the binary image.
     */
    .bss ALIGN(0x1000) : {
        . = ALIGN(4);
        __bss_start = .;
        *(.bss*)
        *(COMMON)
        . = ALIGN(4);
        __bss_end = .;
    }

    /* End of allocated memory - for heap/memory management */
    _end = .;
    PROVIDE(end = .);

    /* Discard unneeded sections to reduce binary size */
    /DISCARD/ : {
        *(.comment)
        *(.eh_frame)
        *(.note*)
    }
}
