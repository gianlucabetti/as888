000100010925      *PARMS DYNUSRPRF(*OWNER)
000200080220      /TITLE Controllo attività aggiornamento Laguna
000300990908     H dftactgrp(*yes)
000400990907
000500990907
000600080221
000700080221     D*---------------------
000800080221     D* DS "XSRDA8" - CONTROLLA DATA (8)
000900080221     D*---------------------
001000080221     D WLBDA8          DS
001100080221     D  G08DAT                 1      8  0
001200080221     D  G08INV                 9     16  0
001300080221     D  G08ERR                17     17
001400080221     D  G08TGI                18     22  0
001500080221
001600080221
001700080221     D*---------------------
001800080221     D* DS "X16GGSET" - GIORNO DELLA SETTIMANA
001900080221     D*---------------------
002000080221     D X16GGSETDS    E DS
002100080221
002200080221
002300080221     D*---------------------
002400080221     D* VARIABILI D WRK
002500080221     D*---------------------
002600080221     D In_save_upd     S             13
002700080221
002800080221
002900080221     C*
003000080221     C* Reperisco la data/ora ultimo aggiornamento file
003100080221     C                   call      'TRTCLC1CHK'
003200080221     C                   parm                    In_file          10
003300080221     C                   parm                    In_last_upd      13
003400080221     C*
003500080221     C* Reperisco la data e l'ora corrente
003600080221     C                   TIME                    N14              14 0
003700080221     C                   MOVE      N14           N8                8 0
003800080221     C                   MOVEL     N14           ORACOR            6 0
003900080221     C                   Z-ADD     N8            G08DAT
004000080221     C                   Z-ADD     *ZEROS        G08INV
004100080221     C                   MOVEL     '0'           G08ERR
004200080221     C                   CALL      'XSRDA8'
004300080221     C                   PARM                    WLBDA8
004400080221     C                   Z-ADD     G08INV        DATCOR            8 0
004500040309     C*
004600080221     C* Reperisco il giorno della settimana
004700080221     C                   clear                   X16GGSETDS
004800080221     C     *ISO          move      DATCOR        X16DATA
004900080221     C                   call      'X16GGSET'
005000080221     C                   parm                    X16GGSETDS
005100080221     C*
005200080221     C* Effettuo il controllo solo: Lun-Ven@07:30-22:00
005300080221     C                   if        X16GIORNO >= 2 AND X16GIORNO <= 6
005400080221     C                   if        ORACOR >= 073000 AND ORACOR <= 220000
005500080221     C*
005600080221     C                   if        In_last_upd > In_save_upd
005700080221     C                   eval      In_save_upd = In_last_upd
005800080220     C                   else
005900040309     C                   exsr      sndeml
006000050822     C                   endif
006100080221     C*
006200080221     C                   endif
006300080221     C                   endif
006400080220     C*
006500040309     C                   return
006600040309
006700040309
006800040309
006900040309
007000040309      /TITLE Compongo il testo e spedisco una e-m@ail
007100040309     C     sndeml        begsr
007200040309     C*
007300040309     C* Inizializzo variabili
007400040309     C                   movel     *blanks       wrkEml          253
007500040309     C                   movel     *blanks       wrkMsg         5000
007600040309     C                   movel     *blanks       wrkOgg           44
007700080220     C*
007800040309     C* Valorizzo i campi della e-m@ail
007900120305     C                   eval      wrkEml='cedalert@brt.it'
008000080220     C                   eval      wrkOgg='LAGUNA - WARNING!!!'
008100080221     C                   eval      wrkMsg='ATTENZIONE: '                +':/N'+
008200080221     C                             'il file ' + %trim(In_file)          +
008300080221     C                             ' non risulta aggiornato da oltre '  +
008400080221     C                             %editc(In_attesa/60:'4') + ' minuti.'+':/N'+
008500080221     C                             'Verificare corretta attività su'    +
008600080221     C                             ' CEDLAGBO'                          +':/N'
008700040309     C*
008800040309     C                   call(e)   'TIS701C'
008900040309     C                   parm                    wrkEml
009000040309     C                   parm                    wrkOgg
009100040309     C                   parm                    wrkMsg
009200040309     C*
009300040309     C                   endsr
009400000714
009500080220
009600080220
009700000714
009800000714      /TITLE Routine di *init PGM
009900000714     C*
010000000714     C     *inzsr        begsr
010100080220     C*
010200080220     C     *entry        plist
010300080221     C                   parm                    In_file          10
010400080221     C                   parm                    In_attesa        15 5
010500000714     C*
010600000714     C                   endsr
