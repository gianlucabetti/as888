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
001201080411     D xchkautds     e ds
001202080411     D conpnods      e ds
001300981019     d dataiso         s               d   datfmt(*iso)
001400981019     d dataeur         s               d   datfmt(*eur)
001401110110     d savdft          s              8  0
001500981019    >D Societa         S              3
001600981019    >D Ctb             S              2
001700981019     D TpReg           S              1
001800981019     D DtReg           S             10
001900981019     D DtLGio          S             10
002000981019     D Esito           S              1
002100981019     D Gesmsg          S              1
002200000000     C*---------------------------------------------------------------*
002300000000     C     *ENTRY        PLIST
002400000000     C                   PARM                    KPJBA
002500981019     C*---------- RICERCA DITTA :
002600981019     C                   MOVEL     'SOC001'      TIPXSC
002700981019     C                   MOVEL     *BLANK        SOCXSC
002800981019     C                   EXSR      REPSOC
002900981019     C     RTNXSC        IFNE      '1'
003000981019     C                   MOVEL     XSOCDS        SOC001
003100981019     C                   MOVEL     xscrgs        RSUT             20
003200981019     c                   end
003300000000     C*---------------------------------------------------------------*
003400000000     C     STARTX        TAG
003500080411     C                   SETOFF                                       99
003600000000     C                   MOVEL     *ZEROS        PARAM
003601110110     C* RECUPERO LA DATA DEL GIORNO PER IMPOSTARE LA DATA FINE MESE PRECEDENTE
003602110110     C                   TIME                    W0140            14 0
003603110110     C                   MOVE      W0140         WMMAAAA           6 0
003604110110     C                   MOVEL     01            WDTGIO            8 0
003605110110     C                   MOVE      WMMAAAA       WDTGIO
003606110110     C* ROVESCIO LA DATA
003607110110     C                   MOVE      WDTGIO        DATAEUR
003608110110     C                   MOVE      DATAEUR       DATAISO
003609110110     C                   SUBDUR    1:*DAYS       DATAISO
003610110110     C                   MOVE      DATAISO       DATAeur
003611110110     C                   MOVE      DATAeur       DATAMAX           8 0
003612110202     C                   MOVE      DATAEUR       VIDDFT
003700000000     C     START         TAG
003800000121     C                   EXFMT     tnsfD01
003900980527     C** F3 - FINE LAVORO
004000980527     C   KC              GOTO      FINE
004100980527     C** F10 - GESTIONE LAVORO
004200981021     C   kj              MOVEL     '3'           KRITB
004300980527     C   KJ              CALL      'BCH09'
004400000000     C                   PARM                    KPJBA
004500980527     C   KJ              GOTO      START
004501080411      *-------------------------------------------------------------------*
004502080411      * verifico se sono un utente autorizzato a lanciare fatturazione    *
004503080411      *-------------------------------------------------------------------*
004504080411
004505080411     c                   clear                   xchkautds
004506080411     c                   eval      xcasoc = xscsoc
004507080411     c                   eval      xcagrp = xscgrp
004508080411     c                   eval      xcafnc = 'CONPNO'
004509080411     c                   eval      xcaprf = knmus
004510080411     c                   eval      xcatck = 'AP'
004511080411     c                   eval      xcacut = xsccut
004512080411     c                   eval      xcauff = xscuff
004513080411     c                   eval      xcalin = xsculm
004514080411     c                   call      'XCHKAUT'
004515080411     c                   parm                    XCHKAUTDS
004516080411      *
004517080411      * verifico il codice di ritorno  se diverso da zero segnalo l'errore
004518080411     c                   eval      *in57 = xcartn <> 0
004519080411     c   57              goto      start
004520080411      *
004521080411      * se non risultano errori dal richiamo di xchkautds verifico i flag di abilitazione
004522080411      * immissione dati iva,non iva ed effetti
004523080411     c                   movel     xcadds        conpnods
004524080411     c                   eval      *IN57 = (ImmIVA <> '1' or ImmNON <> '1' or
004525080411     c                                      ImmATT <> '1')
004526080411     c   57              goto      start
004527080411
004600981021     c     *eur          test(d)                 viddft                 99
004700981021     c                   if        *in99
004800981019     c     *dmy          test(d)                 viddft                 99
004900981019     c  n99*dmy          move      viddft        dataeur
005000981021     c  n99              move      dataeur       viddft
005004110111     c                   If        not *in99 and viddft <> datamax
005005110111     c                             and viddft <> savdft
005006110110     c                   seton                                          50
005007110110     c                   eval      savdft = viddft
005008110110     c                   move      viddft        dataeur
005009110110     c                   goto      start
005010110110     c                   endif
005100981021     c                   else
005200981021     c                   move      viddft        dataeur
005300981021     c                   end
005400941128     C** DATA ELABORAZIONE
005500000000     C   99              GOTO      START
005600981021     c                   move      dataeur       dataiso
005700000000     C** DATA ELABORAZIONE ERRATA
005800981019     c                   move      dataiso       dtreg
005900981019     c                   move      dataiso       dtlgio
006000981019     C                   CALLB     'NDMVC106'
006100981019     C                   PARM      xscsoc        Societa
006200981019     C                   PARM      'CG'          CTB
006300981019    >C                   PARM      'D'           TpReg
006400981019    >C                   PARM                    DtLGio
006500981019    >C                   PARM                    DtReg
006600981019     C                   PARM      *on           GesMsg
006700981019     C                   PARM                    Esito
006800981019     c                   if        esito = *on
006900981019     C                   seton                                        97
007000910114     C   97              GOTO      START
007100981019     c                   end
007200910114     C**DATA REG. MINORE ULTIMO BOLLATO
007300910114     C** DATA REGISTRAZIONE ERRATA
007400910114     C  N99
007500910114     CANN97              SETON                                        98
007600980527     C   98
007700980527     CANNKF              GOTO      START
007800000000     C     GIU01         TAG
007900981020     c                   move      xscsoc        wsocieta
008000000121     C   KF              MOVEL     'SF04'        KCOAZ
008100980527     C   KF              MOVEL     PARAM         KPJBU
008200001221     C                   CALL      'BCH10'
008300001221     c****               CALL      'TNSF04C'
008400000000     C                   PARM                    KPJBA
008500980527     C**
008600000000     C     FINE          TAG
008700000000     C                   SETON                                        LR
008800981019     C*----------------------------------------------------*
008900981019     C* Reperimento dati società
009000981019     C*----------------------------------------------------*
009100981019     C     REPSOC        BEGSR
009200981019     C*
009300981019     C                   CALL      'XSOC'
009400981019     C                   PARM                    TIPXSC            6
009500981019     C                   PARM                    SOCXSC            3
009600981019     C                   PARM                    CDSXSC            9 0
009700981019     C                   PARM                    MODXSC            3
009800981019     C                   PARM      *blanks       RTNXSC            1
009900981019     C                   PARM                    XSOCDS
010000981019     C                   PARM                    KPJBA
010100981019     C*
010200981019     C                   ENDSR
