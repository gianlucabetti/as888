000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200000000      *****************************************************************
000300000000      *
000400030203      *  Descrizione   :  Stampa tariffe Cooperative
000500000000      *                   Programma di lancio stampa (filtro)
000600000000      *****************************************************************
000700090619     Fficn27D   CF   E             WORKSTN
000701090721     fazorg01l  if   e           k disk
000800030205      *
000900000000     D KPJBA         E DS
001000030203     D Tibs36ds      E DS
001001090721     D ficn27ds      E DS
001100000000     D CNCR80        E DS
001200000000     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
001300940926     D*
001401090721     d trul06ds      e ds
001402090721     d  lint                   1     90  0 dim(30)
001500940926     D*
001600090721     D WLBDA8          DS
001700940926     D  G02DAT                 1      8  0
001800940926     D  G02INV                 9     16  0
001900940926     D  G02ERR                17     17
002000940926     D  G02TGI                18     22  0
002100000000      *
002200000000     C     *ENTRY        PLIST
002300000000     C                   PARM                    KPJBA
002301090721     C                   TIME                    WHHDAT           14 0
002302090721     C                   MOVE      WHHDAT        G02DAT
002303090721     C                   MOVE      *BLANKS       G02ERR
002304090721     C                   CALL      'XSRDA8'
002305090721     C                   PARM                    WLBDA8
002400030203      *
002500030203      *  In base all'utente prende il P.O.
002600030203     C                   CLEAR                   Tibs36Ds
002700030203     c                   EVAL      I36ute = Knmus
002800030203     c                   EVAL      I36Tla = 'L'
002900030203     C                   CALL      'TIBS36R'
003000030203     C                   PARM                    Tibs36Ds
003100030203      * se utente di sede
003300090721     c                   if        o36pou <>046
003402090721     c                   clear                   trul06ds
003403090721     c                   eval      d06cod = '£1'
003404090721     c                   movel     o36pou        d06key
003405090721     c                   move      'S'           d06key
003406090721     C                   Z-ADD     G02INV        d06drf
003407090721     c                   movel     trul06ds      kpjbu
003408090721     c                   call      'TRUL06R'
003409090721     c                   parm                    kpjba
003410090721     c                   movel     kpjbu         trul06ds
003411090721     c* se unico p.o. sottometto subito
003412090721     c                   if        lint(2) = 0
003413090721     c                   clear                   ficn27ds
003414090721     c                   eval      d27fil = o36pou
003415090721     c                   movel     ficn27ds      kpjbu
003416090721     c                   eval      kcoaz ='FN28'
003417090721     C                   CALL      'BCH10'
003418090721     C                   PARM                    KPJBA
003419090721     c                   seton                                        lr
003420090721     c                   return
003500030203     c                   end
003501090721     c                   end
003600000000      *--------------------------------------------------------------*
003700000000      *             M A I N      L I N E
003800000000      *--------------------------------------------------------------*
003900000000     C                   Z-ADD     1             CODUT
004000000000     C                   CALL      'X§PARUT'
004100000000     C                   PARM                    UT§DSE
004200000000     C                   MOVEL     RAGUT         RSUT             20
004300000000     C                   MOVEL     RAGUT         RSUT2            20
004400000000     C                   MOVEL     REC80         CNCR80
004500090619     c                   clear                   SOLORISCH
004600090619      *
004700000000      *
004800000000      * Inizializzo i campi a video
004801090721     c                   if        o36pou = 046
004901090721     c                   exsr      srsed
004902090721     c                   else
004903090721     c                   exsr      srfil
004904090721     c                   end
004905090721     C                   SETON                                        LR
004906090721      *--------------------------------------------------------------*
004907090721     c     srsed         begsr
004908090721      *--------------------------------------------------------------*
005000090619     c                   movel     'S'           SOLORISCH
005300000000      * Emetto il formato video
005301090721     c                   do        *hival
005400090619     C                   EXFMT     cn27D01
005500090721     C   KC              leave                                                  F3=USCITA
006400020307      * Richiamo programma di stampa
006401091012     c                   if        vfil > 0
006402091012     c                   exsr      srorg
006404091012     c   90              iter
006406091009     c                   endif
006407090721     c                   if        *inkf
006408090721     c                   clear                   ficn27ds
006409090721     c                   if        SOLORISCH <> 'S'
006410090721     c                   clear                   SOLORISCH
006411090721     c                   end
006500090721     c                   movel     SOLORISCH     d27solor
006501091012     c                   move      vfil          d27fil
006503090721     c                   movel     ficn27ds      kpjbu
006600090622     c                   eval      kcoaz ='FN28'
006700090622     C                   CALL      'BCH10'
006800090622     C                   PARM                    KPJBA
006801090721     c                   leave
006802090721     c                   end
007200000000      *
007300090721     C                   enddo
007301090721     c                   endsr
007302090721      *--------------------------------------------------------------*
007303090721     c     srorg         begsr
007304090721      *--------------------------------------------------------------*
007305090721     c                   setoff                                       90
007306090721     c     vfil          chain     azorg01l
007307090721     c                   if        %found(azorg01l)
007308090721     c                   eval      desfil = orgdes
007309090721     c                   else
007310090721     c                   clear                   desfil
007311090721     c                   seton                                        90
007312090721     c                   end
007313090721     c                   endsr
007314090721      *--------------------------------------------------------------*
007315090721     c     srfil         begsr
007316090721      *--------------------------------------------------------------*
007317090721     c                   movel     o36pou        vfil
007318090721     c                   exsr      srorg
007319090721      * Emetto il formato video
007320090721     c                   do        *hival
007321090825     c                   if        *in90
007322090825     c                   eval      msgv= 'Filiale errata o non in gestione'
007323090825     c                   else
007324090825     c                   clear                   msgv
007325090825     c                   end
007326090825     C                   EXFMT     cn27D02
007327090721     C   KC              leave                                                  F3=USCITA
007328090721      *
007329090721     c                   clear                   desfil
007331090721     c                   if        vfil = 0
007332090825     c                   seton                                        90
007333090721     c                   iter
007334090721     c                   else
007335090721     c                   exsr      srorg
007336090721     c                   if        *in90
007337090721     c                   iter
007338090721     c                   else
007339090721     c     vfil          lookup    lint                                   88
007340090721     c                   if        not *in88
007341090825     c                   seton                                        90
007342090721     c                   iter
007343090721     c                   end
007344090721     c                   end
007345090721     c                   end
007346090721      * Richiamo programma di stampa
007347090721     c                   if        *inkf
007348090721     c                   clear                   ficn27ds
007349090721     c                   eval      d27fil = vfil
007350090721     c                   movel     ficn27ds      kpjbu
007351090721     c                   eval      kcoaz ='FN28'
007352090721     C                   CALL      'BCH10'
007353090721     C                   PARM                    KPJBA
007354090721     c                   leave
007355090721     c                   end
007356090721      *
007357090721     C                   enddo
007358090721     c                   endsr
007500000000      *--------------------------------------------------------------*
