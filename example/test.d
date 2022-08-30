uartinit() {
    gpiofunc := 0;
    gpiofunc[12:14] := 2;
    gpiofunc[15:17] := 2;
    @538968068 <- gpiofunc;
    devbarrier := @15;
    enb := @539054084;
    enb[0:0] := 1;
    @539054084 <- enb;
    cntl := @539054176;
    cntl[0:1] := 0;
    @539054176 <- cntl;
    iir := @539054152;
    iir[1:2] := 3;
    @539054152 <- iir;
    ier := @539054148;
    ier[0:1] := 0;
    @539054148 <- ier;
    lcr := @539054156;
    lcr[0:1] := 3;
    @539054156 <- lcr;
    baud := 270;
    @539054184 <- baud;
    cntll := @539054176;
    cntll[0:1] := 3;
    @539054176 <- cntll;


    devbarrierone := @15;
}