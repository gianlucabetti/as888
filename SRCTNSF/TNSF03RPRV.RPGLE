000100981019     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PRNPGM) ACTGRP(QILE)
000200981019     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000300981019     H/TITLE  IMPUTAZIONE CONTABILE FATTURE FILIALI
000400941128     H* TNSF03R *----------------------------------------------------*
000500000000     H*--------------------------------------------------------------*
000600000121     FTNSF03D   CF   E             WORKSTN
000700000000     D PARAM           DS
000800981020     D  VIDDFT                        8  0
000900981020     d  wsocieta                      3
001000000000     D KPJBA         E DS
001100981019     D soc001        e ds                  extname(xsoc001ds)
001200981019     D xsocds          DS          1000
001300080411     D xchkautds     e ds
001400080411     D conpnods      e ds
001500120522      * DATA E ORA SCHEDULAZIONE PER KPJBA
001600120522     D SCHEDS          DS
001700120522     D  D1DATA                 1      6
001800120522     D  D1ORAS                 7     10
001900120522     d
002000981019     d dataiso         s               d   datfmt(*iso)
002100981019     d dataeur         s               d   datfmt(*eur)
002200120522     d datasys         s               d   datfmt(*iso) inz(*sys)
002300120522     d datadmy         s               d   datfmt(*dmy)
002400110110     d savdft          s              8  0
002500981019    >D Societa         S              3
002600981019    >D Ctb             S              2
002700981019     D TpReg           S              1
002800981019     D DtReg           S             10
002900981019     D DtLGio          S             10
003000981019     D Esito           S              1
003100981019     D Gesmsg          S              1
003200000000     C*---------------------------------------------------------------*
003300000000     C     *ENTRY        PLIST
003400000000     C                   PARM                    KPJBA
003500981019     C*---------- RICERCA DITTA :
003600981019     C                   MOVEL     'SOC001'      TIPXSC
003700981019     C                   MOVEL     *BLANK        SOCXSC
003800981019     C                   EXSR      REPSOC
003900981019     C     RTNXSC        IFNE      '1'
004000981019     C                   MOVEL     XSOCDS        SOC001
004100981019     C                   MOVEL     xscrgs        RSUT             20
004200981019     c                   end
004300000000     C*---------------------------------------------------------------*
004400000000     C     STARTX        TAG
004500080411     C                   SETOFF                                       99
004600000000     C                   MOVEL     *ZEROS        PARAM
004700110110     C* RECUPERO LA DATA DEL GIORNO PER IMPOSTARE LA DATA FINE MESE PRECEDENTE
004800110110     C                   TIME                    W0140            14 0
004900110110     C                   MOVE      W0140         WMMAAAA           6 0
005000110110     C                   MOVEL     01            WDTGIO            8 0
005100110110     C                   MOVE      WMMAAAA       WDTGIO
005200110110     C* ROVESCIO LA DATA
005300110110     C                   MOVE      WDTGIO        DATAEUR
005400110110     C                   MOVE      DATAEUR       DATAISO
005500110110     C                   SUBDUR    1:*DAYS       DATAISO
005600110110     C                   MOVE      DATAISO       DATAeur
005700110110     C                   MOVE      DATAeur       DATAMAX           8 0
005800110202     C                   MOVE      DATAEUR       VIDDFT
005900000000     C     START         TAG
006000000121     C                   EXFMT     tnsfD01
006100980527     C** F3 - FINE LAVORO
006200980527     C   KC              GOTO      FINE
006300980527     C** F10 - GESTIONE LAVORO
006400981021     C   kj              MOVEL     '3'           KRITB
006500980527     C   KJ              CALL      'BCH09'
006600000000     C                   PARM                    KPJBA
006700980527     C   KJ              GOTO      START
006800080411      *-------------------------------------------------------------------*
006900080411      * verifico se sono un utente autorizzato a lanciare fatturazione    *
007000080411      *-------------------------------------------------------------------*
007100080411
007200080411     c                   clear                   xchkautds
007300080411     c                   eval      xcasoc = xscsoc
007400080411     c                   eval      xcagrp = xscgrp
007500080411     c                   eval      xcafnc = 'CONPNO'
007600080411     c                   eval      xcaprf = knmus
007700080411     c                   eval      xcatck = 'AP'
007800080411     c                   eval      xcacut = xsccut
007900080411     c                   eval      xcauff = xscuff
008000080411     c                   eval      xcalin = xsculm
008100080411     c                   call      'XCHKAUT'
008200080411     c                   parm                    XCHKAUTDS
008300080411      *
008400080411      * verifico il codice di ritorno  se diverso da zero segnalo l'errore
008500080411     c                   eval      *in57 = xcartn <> 0
008600080411     c   57              goto      start
008700080411      *
008800080411      * se non risultano errori dal richiamo di xchkautds verifico i flag di abilitazione
008900080411      * immissione dati iva,non iva ed effetti
009000080411     c                   movel     xcadds        conpnods
009100080411     c                   eval      *IN57 = (ImmIVA <> '1' or ImmNON <> '1' or
009200080411     c                                      ImmATT <> '1')
009300080411     c   57              goto      start
009400080411
009500981021     c     *eur          test(d)                 viddft                 99
009600981021     c                   if        *in99
009700981019     c     *dmy          test(d)                 viddft                 99
009800981019     c  n99*dmy          move      viddft        dataeur
009900981021     c  n99              move      dataeur       viddft
010000110111     c                   If        not *in99 and viddft <> datamax
010100110111     c                             and viddft <> savdft
010200110110     c                   seton                                          50
010300110110     c                   eval      savdft = viddft
010400110110     c                   move      viddft        dataeur
010500110110     c                   goto      start
010600110110     c                   endif
010700981021     c                   else
010800981021     c                   move      viddft        dataeur
010900981021     c                   end
011000941128     C** DATA ELABORAZIONE
011100000000     C   99              GOTO      START
011200981021     c                   move      dataeur       dataiso
011300000000     C** DATA ELABORAZIONE ERRATA
011400981019     c                   move      dataiso       dtreg
011500981019     c                   move      dataiso       dtlgio
011600981019     C                   CALLB     'NDMVC106'
011700981019     C                   PARM      xscsoc        Societa
011800981019     C                   PARM      'CG'          CTB
011900981019    >C                   PARM      'D'           TpReg
012000981019    >C                   PARM                    DtLGio
012100981019    >C                   PARM                    DtReg
012200981019     C                   PARM      *on           GesMsg
012300981019     C                   PARM                    Esito
012400981019     c                   if        esito = *on
012500981019     C                   seton                                        97
012600910114     C   97              GOTO      START
012700981019     c                   end
012800910114     C**DATA REG. MINORE ULTIMO BOLLATO
012900910114     C** DATA REGISTRAZIONE ERRATA
013000910114     C  N99
013100910114     CANN97              SETON                                        98
013200980527     C   98
013300980527     CANNKF              GOTO      START
013400000000     C     GIU01         TAG
013500981020     c                   move      xscsoc        wsocieta
013600000121     C   KF              MOVEL     'SF04'        KCOAZ
013700980527     C   KF              MOVEL     PARAM         KPJBU
013800120906     C*****              CALL      'BCH10'
013900120906     c                   CALL      'TNSF04C'
014000000000     C                   PARM                    KPJBA
014100120522
014200120522     c** Sottometto azione schedulata aggiornamneti automatici p.iva e cod.fiscale clienti vari
014300120523     c                   clear                   kpjbu
014400120522     C                   MOVEL     'Y60S'        KCOAZ
014500120522     c                   eval      d1oras='2300'
014600120522     c* Data   del  giorno
014700120522     c                   movel     datasys       datadmy
014800120522     c     *dmy          movel     datadmy       w0060             6 0
014900120522     c                   movel     w0060         D1DATA
015000120522     c                   movel     scheds        kbuff
015100120523     c                   eval      KQIEB='KSNOTTE'
015200120522     c
015300120522     C                   CALL      'BCH10'
015400120522     C                   PARM                    KPJBA
015500980527     C**
015600000000     C     FINE          TAG
015700000000     C                   SETON                                        LR
015800981019     C*----------------------------------------------------*
015900981019     C* Reperimento dati società
016000981019     C*----------------------------------------------------*
016100981019     C     REPSOC        BEGSR
016200981019     C*
016300981019     C                   CALL      'XSOC'
016400981019     C                   PARM                    TIPXSC            6
016500981019     C                   PARM                    SOCXSC            3
016600981019     C                   PARM                    CDSXSC            9 0
016700981019     C                   PARM                    MODXSC            3
016800981019     C                   PARM      *blanks       RTNXSC            1
016900981019     C                   PARM                    XSOCDS
017000981019     C                   PARM                    KPJBA
017100981019     C*
017200981019     C                   ENDSR
