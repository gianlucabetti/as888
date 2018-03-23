000100981019     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PRNPGM) ACTGRP(QILE)
000200981019     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000300981019     H/TITLE  IMPUTAZIONE CONTABILE FATTURE FILIALI
000400941128     H* TNSF03R *----------------------------------------------------*
000500000000     H*--------------------------------------------------------------*
000600981019     FYNSF03D   CF   E             WORKSTN
000700000000     D PARAM           DS
000800981020     D  VIDDFT                        8  0
000900981020     d  wsocieta                      3
001000000000     D KPJBA         E DS
001100981019     D soc001        e ds                  extname(xsoc001ds)
001200981019     D xsocds          DS          1000
001300981019     d dataiso         s               d   datfmt(*iso)
001400981019     d dataeur         s               d   datfmt(*eur)
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
003500000000     C                   SETOFF                                       99
003600000000     C                   MOVEL     *ZEROS        PARAM
003700000000     C     START         TAG
003800981019     C                   EXFMT     ynsfD01
003900980527     C** F3 - FINE LAVORO
004000980527     C   KC              GOTO      FINE
004100980527     C** F10 - GESTIONE LAVORO
004200981021     C   kj              MOVEL     '3'           KRITB
004300980527     C   KJ              CALL      'BCH09'
004400000000     C                   PARM                    KPJBA
004500980527     C   KJ              GOTO      START
004600981021     c     *eur          test(d)                 viddft                 99
004700981021     c                   if        *in99
004800981019     c     *dmy          test(d)                 viddft                 99
004900981019     c  n99*dmy          move      viddft        dataeur
005000981021     c  n99              move      dataeur       viddft
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
008000981019     C   KF              MOVEL     'YF04'        KCOAZ
008100980527     C   KF              MOVEL     PARAM         KPJBU
008200950404     C                   CALL      'BCH10'
008300000000     C                   PARM                    KPJBA
008400980527     C**
008500000000     C     FINE          TAG
008600000000     C                   SETON                                        LR
008700981019     C*----------------------------------------------------*
008800981019     C* Reperimento dati società
008900981019     C*----------------------------------------------------*
009000981019     C     REPSOC        BEGSR
009100981019     C*
009200981019     C                   CALL      'XSOC'
009300981019     C                   PARM                    TIPXSC            6
009400981019     C                   PARM                    SOCXSC            3
009500981019     C                   PARM                    CDSXSC            9 0
009600981019     C                   PARM                    MODXSC            3
009700981019     C                   PARM      *blanks       RTNXSC            1
009800981019     C                   PARM                    XSOCDS
009900981019     C                   PARM                    KPJBA
010000981019     C*
010100981019     C                   ENDSR
