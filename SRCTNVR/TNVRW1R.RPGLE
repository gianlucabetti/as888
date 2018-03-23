000100990908     H dftactgrp(*yes)
000200990908
000300040324     Fwfwu100f  uF   E             DISK
000400990908
000500040318     D tisi95ds      e ds
000600040324     D fnlv55ds      e ds
000700010606     D*------------------
000800010606     D* DS "XSRDA8" - CONTROLLA DATA (8)
000900010606     D*------------------
001000010606     D WLBDA8          DS                  INZ
001100010606     D  G08DAT                 1      8  0
001200010606     D  G08INV                 9     16  0
001300010606     D  G08ERR                17     17
001400010606     D  G08TGI                18     22  0
001500010606     D*
001600990908
001700010530     C*
001800040324     C                   exsr      exeElab
001900010530     C*
002000010530     C                   seton                                        lr
002100990908
002200010530
002300010530
002400010530
002500910830     C*--------------------------------------------------------
002600040324     C* EXEELAB - LEGGE IL FILE D INPUT E RICAVA LNA E TFA    *
002700910830     C*--------------------------------------------------------
002800040324     C     EXEELAB       BEGSR
002900990910     C*
003000040324     C                   do        *HIVAL
003100040324     C                   read      WFWU100F                               70
003200990913     C*
003300040324     C                   if        *in70 = *off
003400040324     C*
003500040324     C* Inizializzo la DS d procedura del TISI95 x il reperimento LNA
003600040324     C                   clear                   TISI95DS
003700040324     C                   eval      I95TCN = '3'
003800040324     C                   z-add     datcor        I95DAT
003900040324     C                   eval      I95CAP = WU1CAP
004000040324     C                   eval      I95PRV = WU1PRV
004100990913     C*
004200040324     C* Trasformo in numerico il peso e lo esprimo in kg.
004300040324     C                   eval      PiStr = WU1PES
004400040324     C                   exsr      CHKNUM
004500040324     C                   if        PiInt=*on
004600040324     C                   eval      I95LKG = PiVal / 1000
004700040324     C*
004800040324     C* Chiamo il TISI95 x il reperimento dei dati d instradamento
004900040324     C                   call      'TISI95R'
005000040324     C                   parm                    TISI95DS
005100040324     C*
005200040324     C* Se nessun errore in reperimento dati d instradamento procedo con la valorizzazione
005300040324     C* dei campi d output
005400040324     C                   if        O95ERR = *blanks
005500040324     C                   movel     O95LNA        WU1LNA
005600040324     C*
005700040324     C* Una volta reperita la LNA chiamo l'FNLV55 x il reperimento del relativo terminal
005800040324     C                   clear                   FNLV55DS
005900040324     C                   movel     'A'           D55TPT
006000040324     C                   movel     WU1LNA        D55LIN
006100040324     C                   z-add     datcor        D55DRF
006200040324     C                   call      'FNLV55R'
006300040324     C                   parm                    FNLV55DS
006400040324     C                   if        D55ERR = *blanks
006500040324     C                   movel     D55TFA        WU1TFA
006600040324     C                   endif
006700040324     C                   endif
006800040324     C                   endif
006900040324     C*
007000040324     C* Al termine dei reperimenti aggiorno il file con i campi d output
007100040324     C                   update    WFWU1000
007200040324     C                   endif
007300991022     C*
007400040324     C* Proseguo con la lettura sequenziale d tutto il file d input
007500040324     C  N70              enddo
007600990920     C*
007700910830     C                   ENDSR
007800000613     C***
007900040316
008000040316
008100040316
008200040316     C*----------------------------------------------------*
008300040316     C*  CONTROLLO NUMERICITA' CAMPI
008400040316     C*----------------------------------------------------*
008500040316     C     CHKNUM        BEGSR
008600040316     C*
008700040316     C                   IF        PiDecChr = *blanks
008800040316     C                   EVAL      PiDecChr = ','
008900040316     C                   ENDIF
009000040316     C*
009100040316     C                   CALL(e)   'ISNUMERIC'
009200040316     C                   PARM                    PiStr            30
009300040316     C                   PARM                    PiDecChr          1
009400040316     C                   PARM      *ZEROS        PiVal            30 9
009500040316     C                   PARM      '0'           PiInt             1
009600040316     C                   PARM      '0'           PiNum             1
009700040316     C                   IF        %error
009800040316     C                   EVAL      PiNum=*off
009900040316     C                   ENDIF
010000040316     C*
010100040316     C                   ENDSR
010200040316     C***
010300010530
010400010530
010500010530
010600990910
010700000613     C     *inzsr        BEGSR
010800010606     C*
010900010606     C* CALCOLA LA DATA CORRENTE
011000010606     C                   time                    wn14             14 0
011100010606     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
011200010606     C                   z-add     wn8           g08dat
011300010606     C                   z-add     *zeros        g08inv
011400010606     C                   movel     '0'           g08err
011500010606     C                   call      'XSRDA8'
011600010606     C                   parm                    wlbda8
011700010606     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
011800000613     C*
011900000613     C                   ENDSR
