000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200000000      *****************************************************************
000300000000      *
000400081002      *  Descrizione   :  Stampa tariffe aff/def
000500000000      *                   Programma di lancio stampa (filtro)
000600000000      *****************************************************************
000700090408     Ftrmz79D   CF   E             WORKSTN
000800090408     Fazorg01L  IF   E           K DISK
000900000000      *
001000940926     D*
001100000000     D KPJBA         E DS
001200000000     D CNCR80        E DS
001300090408     D trmz77ds      E DS
001400090408     D daua          E DS
001500000000     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
001600020307     D dsparam         DS
001700090408     D  parfil                        3s 0
001800090423     D  pardat                        8s 0
001900090428     D  paroutq                      10
002000090423     d dataiso         s               d   datfmt(*iso)
002100090423     d udateiso        s               d   datfmt(*iso)
002200090423     d dataeur         s               d   datfmt(*eur)
002300090423     D WLBDA8          DS
002400090423     D  G02DAT                 1      8  0
002500090423     D  G02INV                 9     16  0
002600090423     D  G02ERR                17     17
002700090423     D  G02TGI                18     22  0
002800090428     d lung            S             15  5 inz(37)
002900090428     d comman          S             80
003000090423      *
003100090428     D CM              S              1    DIM(37) CTDATA PERRCD(37)
003200060320      *__________________________________________________________________
003300000000     C     *ENTRY        PLIST
003400000000     C                   PARM                    KPJBA
003500090423     C                   time                    w0120            14 0
003600090423     C                   move      w0120         wdat              8 0
003700090423      * data da impostare su data stampa e convalida se lancio = 'C'
003800090423     c                   move      wdat          dataeur
003900090423     c                   move      dataeur       udateiso
004000090424     c                   move      dataeur       dataiso
004100090424     c                   move      wdat          viddat
004200090423      *
004300090408     C                   SETON                                        LR
004400000000      *
004500000000      *--------------------------------------------------------------*
004600000000      *             M A I N      L I N E
004700000000      *--------------------------------------------------------------*
004800000000     C                   Z-ADD     1             CODUT
004900000000     C                   CALL      'X§PARUT'
005000000000     C                   PARM                    UT§DSE
005100000000     C                   MOVEL     RAGUT         RSUT             20
005200000000     C                   MOVEL     RAGUT         RSUT2            20
005300000000     C                   MOVEL     REC80         CNCR80
005400000000      *
005500081119      * controllo autorizzazione del profilo ad accedere alle tariffe AFF/DEF
005600090408     c                   clear                   daua
005700090408     c                   clear                   trmz77ds
005800090408     c                   movel     knmus         i77prf
005900090408     c                   call      'TRMZ77R'
006000090408     c                   parm                    trmz77ds
006100090408     c                   movel     o77uni        daua
006200090408     c                   if        §AUAGES <> 'S'
006300090408     c                   return
006400090408     c                   endif
006500000000      * Inizializzo i campi a video
006600090408     C                   Z-ADD     *ZERO         VIDfil
006700000000      *
006800000000      * Emetto il formato video
006900060324     c                   do        *hival
007000000000     C     EMETTO        TAG
007100020308     C                   SETOFF                                       90
007200090408     C                   EXFMT     mz79D01
007300000000      *
007400000000     C   KC              GOTO      FINE                                         F3=USCITA
007500000000      *
007600000000      * Eseguo i  controlli sui campi immessi a video
007700000000     C                   EXSR      CONTR
007800000000      *
007900020308      * Se non vi sono errori ma non e' premuto F6 emetto video
008000020308     C  N90
008100020308     CanNkf              SETON                                          90      F6=CONFERMA
008200000000      *
008300000000      * Per errore generico riemetto il formato video
008400000000     C   90              GOTO      EMETTO
008500000000      *
008600020307      * Richiamo programma di stampa
008700020308     c                   if        *inkf
008800120306      **
008900120306      **   AGGIUNTO aggiornamento delle tariffe AFFL/DEFL con contratto =(0)
009000120306      **    di TRAZIONISTI --> il contratto e la filiale sulle tariffe
009100120306      **    devono essere 9999999 : questo programma esegue l'aggiornamento.
009200120511     c***************
009300120511      ********* TOLTO perchè serviva solo nel momento della trascodifica
009400120511      *********       adesso invece genera il 9999999 erroneamente.
009500120511     c***************    call      'TRMZA3R0'
009600120511     c***************    parm                    kpjba
009700120306      **   poi prosegue con le stampe di controllo
009800120306      *
009900090408     c                   move      vidfil        parfil
010000090430     c                   move      voutq         paroutq
010100090428     c                   move      dataiso       pardat
010200020307     c                   movel     dsparam       kpjbu
010300090408     C                   CALL      'TRMZ79R1'
010400000000     C                   PARM                    KPJBA
010500090831      * eventuali stampe di recupero
010600090902     c                   movel     dsparam       kpjbu
010700090831     C                   CALL      'TRMZ79R2'
010800090831     C                   PARM                    KPJBA
010900090114     c                   goto      fine
011000020308     c                   endif
011100060324     c                   enddo
011200000000      *
011300000000     C     FINE          TAG
011400090408     c                   return
011500000000      *--------------------------------------------------------------*
011600000000      * Routine per i controlli sui campi del formato video          *
011700000000      *--------------------------------------------------------------*
011800000000     C     CONTR         BEGSR
011900000000      *
012000090423     C                   SETOff                                       454647
012100090428     C                   SETOff                                       4841
012200111206     c* filiale obbligatoria solo per ristampa di tutti gli alllegati e
012300111206     c* contratti
012400120321     c                   if        vidfil = 0
012500120321     C                   SETON                                        45  90
012600120321     c                   leavesr
012700120321     c                   endif
012800111215     c                   if        vidfil <> 0
012900090408     C     vidfil        chain     azorg01l
013000090408     c                   if        %found(azorg01l)
013100090408     C                   MOVEL     orgdes        vidfild
013200060320     c                   else
013300060320     C                   SETON                                        45  90
013400090423     c                   leavesr
013500060320     c                   endif
013600090424     c                   endif
013700090423      * controlli data certa
013800090424     c                   if        viddat <> wdat
013900090424     C                   MOVE      Viddat        G02DAT
014000090423     C                   MOVEL     *BLANK        G02ERR
014100090423     C                   CALL      'XSRDA8'
014200090423     C                   PARM                    WLBDA8
014300090423     C     G02ERR        IFEQ      '1'
014400090423     C                   SETON                                        4690
014500090423     c                   leavesr
014600090423     C                   END
014700090424     c                   move      g02dat        viddat
014800090423     c                   move      g02inv        dataiso
014900090423     c                   endif
015000090423      * data certa minore di udate non possibile
015100090423     c                   if        dataiso  < udateiso
015200090423     C                   SETON                                        4790
015300090423     c                   leavesr
015400090423     c                   endif
015500000000      *
015600090428      * data certa maggiore di udate di 7 giorni
015700090423     c     dataiso       subdur    udateiso      giorni:*d         5 0
015800090423     C                   if        giorni > 7
015900090423     C                   SETON                                        4890
016000090423     c                   leavesr
016100090423     c                   endif
016200090428     c                   if        voutq <> *blank
016300090428     C                   MOVEA     Voutq         CM(12)
016400090428     C                   MOVEA     CM            COMMAN
016500090428     C                   CALL      'QCMDEXC'                            41
016600090428     C                   PARM                    COMMAN
016700090428     C                   PARM                    LUNG
016800090428     c                   endif
016900090428     c   41              seton                                        90
017000090423      *
017100000000     C     ENDCTL        ENDSR
017200090428**
017300090428CHKOBJ OBJ(XXXXXXXXXX) OBJTYPE(*OUTQ)
