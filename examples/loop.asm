define counter $00  ; Address of counter
define max 10       ; Num of times to loop

.org $8000

main:
  jsr init          ; Initialize counter
  jsr loop          ; Run loop
  jmp exit          ; Exit program

init:
  lda #0            ; Load 0 into accumulator
  sta counter       ; Store accumulator into counter address
  rts

loop:
  lda counter       ; Load what's in counter address into accumulator
  cmp #max          ; Compare accumulator to max value
  beq done          ; If equal, branch to done
  inc counter       ; Increment counter
  jmp loop          ; Jump to loop
done:
  rts               ; Return back to main

exit:
