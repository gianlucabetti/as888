000100990506     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PJXBND PJCBND)
000200990506     H*PARMS ACTGRP(QILE)
000300980910     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000500951002?     *--------------------------------------------------------------*
000700951002      *     *----------------------------------------------*         *
000800031202      *      contabilizzaz. autofatturaz. extra                      *
000900031119      *     *----------------------------------------------*         *
001100951002?     *--------------------------------------------------------------*
001200951002      * KC - FINE LAVORO                                             *
001300951002?     *--------------------------------------------------------------*
001400951002      * 28 - Visualizzazione messaggio di errore                     *
002700951002?     *--------------------------------------------------------------*
003400020417     Ftntbe01l  IF   E           K DISK
003600031121     Fficn95D   CF   E             WORKSTN
007800980910?     *--------------------------------------------------------------*
007900980910?     *  DS                                                          *
008000980910?     *--------------------------------------------------------------*
008001031121     D param           DS
008002031121     D  tipo                          1
008003031121     D  causale                       4
008100980910     D DSFMT           DS
008200980910     D  $TASTO               369    369
008300980910     D  NRG                  370    370
008400980910     D  NCL                  371    371
008600980910     D* posizione cursore
008700980910     D CURSOR          DS
008800980910     D  RIRI                   1      2B 0 INZ
008900980910     D  R$$                    2      2
009000980910     D  COCO                   3      4B 0 INZ
009100980910     D  C$$                    4      4
009600951002     D KPJBA         E DS
009602031119     D tibs02ds      E DS
010505031119     D dcaf          E DS
010506031124     D ddatiute      e ds
010507031124     D azuteds       e ds                  extname(AZUTE00F)
010508031124     D tibs34ds      E DS                  inz
012901031119     D* Reperimento nome PGM
012902031119     D                SDS
012903031119     D  nompgm           *PROC
013400951002?     *--------------------------------------------------------------*
013500951002?     *  CICLO PRINCIPALE                                            *
013600951002?     *--------------------------------------------------------------*
013700951002      *  Impostazione parametri prima videata
013800951002     C                   EXSR      INZ01
013900951002      *  Loop gestione videata
014000031120     C                   DO        *hival
014100951002     C                   EXSR      GESD01
014200031120     C                   ENDdo
014600951002?     *--------------------------------------------------------------*
014700951002?     *  INZ01: Inizializzazione prima videata                       *
014800951002?     *--------------------------------------------------------------*
014900951002     C     INZ01         BEGSR
015000951002      *
015100031121     C                   SETOFF                                       284948
016900031119     C                   MOVEL     *blanks       v1ctab
016901031119     C                   MOVEL     *blanks       v1dtab
017000951002     C*
017100951002     C                   ENDSR
017200951002?     *--------------------------------------------------------------*
017300951002?     *  GESD01: Gestione prima videata                              *
017400951002?     *--------------------------------------------------------------*
017500951002     C     GESD01        BEGSR
017600951002      *
017601031120     c                   if        *in28
017602031120     c                   setoff                                       28
017700031120     C                   write     y350d01
017701031120     c                   seton                                        28
017702031120     c                   end
017703031120     C*
017704031120     C                   EXFMT     y350d01
017800031121     C                   SETOFF                                       284948
018100951002      *  Fine Lavoro
018200951002     C     *INKC         IFEQ      '1'
018300031120     C                   exsr      fine
018500951002     C                   END
018600031121     C*  effettua ricerche
018700980910     c                   if        *inkd = *on
018800980910     c                   exsr      search
018900020503     C                   GOTO      FINVD1
019000980910     c                   end
019100951002      *  Controlli
019200951002     C                   EXSR      CTR01
019300951002     C   28              GOTO      FINVD1
019400031119      *  No errori
019500031119     C                   EXSR      CALL93
019600951002      *
019700951002     C     FINVD1        ENDSR
019800951002?     *--------------------------------------------------------------*
019900951002?     *  CTR01: Controlli prima videata                              *
020000951002?     *--------------------------------------------------------------*
020100951002     C     CTR01         BEGSR
037301031119      *  Controllo tabella CAF
037302031119     c                   clear                   v1dtab
037303031121     c                   if        v1ctab = ' '
037304031121     c                   seton                                        4928
037305031121     c                   else
037306031119     c                   movel     v1ctab        tbeke1
037307031119     c     ktbe          chain     tntbe01l                           28
037308031120     c                   if        *in28 or tbeatb = 'A'
037309031120     c                   seton                                        4928
037310031119     c                   else
037311031119     c                   movel     tbeuni        dcaf
037312031119     c                   movel     §cafdes       v1dtab
037313031119     c                   end
037314031119     c                   end
049000951003      *
049100031119     C                   ENDSR
057900020404     C************************************************************
058000020404     C* RICERCHE
058100020404     C************************************************************
058200020404     C     SEARCH        BEGSR
058400980910     C* determino Riga/Colonna del cursore
058500980910     C                   MOVE      nrg           R$$
058600980910     C                   MOVE      ncl           C$$
058700980910     C                   Z-ADD     RIRI          CURR
058800980910     C                   Z-ADD     COCO          CURC
059200980910     C*
0593009809101    C                   SELECT
065300031202     C* FMTD1  - ricerca tabella autofatturaz. extra
065400031119    >C     H1NMFL        WHEneq    'V1CTAB'
065401031119     C                   clear                   Tibs02DS
065402031119     C                   eval      t02tla = 'L'
065404031119     C                   movel     'R'           t02mod
065405031119     C                   movel     'CAF'         t02cod
065406031119     C                   CALL      'TIBS02R'
065407031119     C                   parm                    KPJBA
065408031119     C                   parm                    TIBS02DS
065409031119    2C                   IF        T02err = *blanks
065410031119     c                   movel     t02ke1        v1ctab
065411031119     c                   movel     t02uni        dcaf
065412031119     c                   movel     §cafdes       v1dtab
065413031119     c                   end
066800020510     C                   ENDsl
067200980910     C* imposto pos. del cursore
067300980910     C                   Z-ADD     CURR          H1RIGA
067400980910     C                   Z-ADD     CURC          H1COLO
067500980910     C                   ENDsr
067600951006?     *--------------------------------------------------------------*
067700031121?     *  CALL93: Chiama pgm contabilizzaz.                           *
067800951006?     *--------------------------------------------------------------*
067900031119     C     CALL93        BEGSR
068000951006      *
068001031201     C                   movel     §caftip       tipo
068002031124     C                   movel     v1ctab        causale
068003031124     C                   movel(p)  param         kpjbu
068004031124     c                   select
068005031124     c* Contabilizz.
068006031124     c                   when      consta = 'C'
070700031121     C                   CALL      'FICN50C'
070800951006     C                   PARM                    KPJBA
070801031124     c* stampa
070802031124     c                   when      consta = 'S'
070803031124     C                   CALL      'FICN54R'
070804031124     C                   PARM                    KPJBA
070805031124     c* archiviaz. CD
070806031124     c                   when      consta = 'A'
070807031124     C                   CALL      'FICN57C'
070808031124     C                   PARM                    KPJBA
070809031124     c                   endsl
070810031121     C                   exsr      fine
072200951006      *
072300951006     C                   ENDSR
072400980910     C*----------------------------------------------------*
072500031120     C* fine
072600980910     C*----------------------------------------------------*
072700031120     C     fine          begsr
072800980910     C*
072801031120     c                   seton                                        lr
072802031120     c                   return
072803031120     c*
072804031120     C                   ENDSR
073900951006?     *--------------------------------------------------------------*
074000951006?     * *INZSR: Operazioni iniziali                                  *
074100951006?     *--------------------------------------------------------------*
074200951009     C     *INZSR        BEGSR
074300951006      *
074400951006     C     *ENTRY        PLIST
074500951006     C                   PARM                    KPJBA
074501031124     c                   movel     kpjbu         consta            1
074502031124     c     *dtaara       define    §azute        azuteds
074503031124     c     *dtaara       define    §datiute      ddatiute
074504031124     C                   in(E)     *dtaara
074505031124     C                   IF        %Error  or  RSUT = *blanks
074506031124     C                   call      'TIBS34R'
074507031124     C                   parm                    Tibs34Ds
074508031124     C                   in        *dtaara
074509031124     c                   ENDIF
074510031124     c                   select
074511031124     c* Contabilizz.
074512031124     c                   when      consta ='C'
074513031124     c                   eval      operaz = 'CONTABILIZZAZIONE'
074514031124     c* stampa
074515031124     c                   when      consta = 'S'
074516031124     c                   eval      operaz = '      STAMPA     '
074517031125     c* archiviazione
074518031125     c                   when      consta = 'A'
074519031125     c                   eval      operaz = '  ARCHIVIAZIONE  '
074520031124     c                   endsl
084500031119     C     Ktbe          KLIST
084600031119     C                   KFLD                    tbecod
084700031119     C                   KFLD                    tbeke1
084701031119     c                   eval      tbecod='CAF'
085200951006      *  Definisco variabili
085300980910     C                   Z-ADD     0             CURR              2 0
085400980910     C                   Z-ADD     0             CURC              2 0
085700980910      *
085800980910     c                   endsr
