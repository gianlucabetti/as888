000100030718     H DECEDIT('0,') DATEDIT(*DMY.)  OPTION(*NODEBUGIO)
000200051123     H* FNLV80R *----------------------------------------------------*
000300051123     H* - decodifica consegna richiesta
000400000000     H*--------------------------------------------------------------*
000500051129     fFiar501l  if   e           k Disk    usropn
000600051129     fFiar531c  if   e           k Disk    usropn
000700051129     F                                     RENAME(fiar5000:fiar5tas)
000800030722     F**
000900051123     d dVpo          e ds
001000051123     D DAR5GEN       E DS
001100051123     D* DS PER TIBS02R - GESTIONE TNTBE00F
001200051123     D DSBS02        E DS                  EXTNAME(TIBS02DS)
001300030613
001400051123     d kAr5Trd         s                   Like(Ar5Trd) inz('GEN')
001500051123     d Dataiso         s               d   datfmt(*iso)
001600051124     D Datasped        s              8  0
001700941216     D***
001800941216     D* PARAMETRI PER CONTROLLO DATA E TRASFORM IN GIORNI - XSRDA8 -
001900941216     D***
002000941216     D WLBDAT          DS                  INZ
002100941216     D  G02DAT                 1      8  0
002200941216     D  G02INV                 9     16  0
002300941216     D  G02ERR                17     17
002400941216     D  G02TGI                18     22  0
002500901116     D                 DS
002600911024     D KPJBA         E DS
002700051123      * ds di passaggio dati
002800051123     D fnlv80ds      E DS
002900900518     C***************************************************************
003000000000     C     *ENTRY        PLIST
003100000000     C                   PARM                    KPJBA
003200051123     C                   PARM                    fnlv80ds
003300940223     C*---------------------------------------------------------------*
003400051123     C* TIPO LANCIO, ilv80TLA "C" -           CHIUSO CON LR
003500051123     C* TIPO LANCIO, ilv80TLA "L" - ELABORO E CHIUDO CON LR
003600051123     C* TIPO LANCIO, ilv80TLA " " - ELABORO E CHIUDO IN RETRN
003700941216     C*
003800051123    1C     ilv80TLA      IFNE      'C'
003900051123     c                   clear                   olv80err
004000051123     c                   clear                   olv80msg
004100051124     c                   clear                   olv80tcrd
004200051124     c                   clear                   olv80dcr8
004300051124     c                   clear                   olv80dcr6
004400051123     c
004500051123     C* SE NON PASSATA DATA SPEDIZIONE CALCOLO UDATE
004600051123     C                   IF        ILV80MGS=0
004700051123     C                   TIME                    W0140            14 0
004800051123     C* UDATE IN GGMMAAAA
004900051123     C                   MOVE      W0140         WDTGIO            8 0
005000051123     c     *eur          Move      wdtgio        Dataiso
005100051123     c                   Move      Dataiso       DATEU             8 0
005200051123     c                   endif
005300051123     c
005400051123     c
005500941219     C* C'E' LA DATA
005600051123    2C     ilv80dcr      IFGT      0
005700941219     C*
005800051123    3C                   SELECT
005900051123     C     ilv80tcr      WHENEQ    'P'
006000051124     C                   eval      olv80tcrd='PRIMA DEL'
006100051123     C     ilv80tcr      WHENEQ    'D'
006200051124     C                   eval      olv80tcrd='DOPO  IL '
006300941216     C                   OTHER
006400051124     C                   eval      olv80tcrd='      IL '
006500051123     C* VEDO SE TASSATIVA
006600051123    4C                   IF        ILV80TC1='S' OR ILV80TC2='S'
006700051124     c                   movel     ilv80aas      datasped
006800051124     c                   move      ilv80mgs      datasped
006900051123     c* solo se già partita la procedura
007000051123    5c                   if        (ilv80mgs>0 and datasped>=§vpodcrta) or
007100051123     c                             (ilv80mgs=0 and dateu>=§vpodcrta)
007200051123     C* solo se immessa in partenza
007300051129     c                   if        %open(fiar501l)
007400051123     c     kar5          chain     fiar501l
007500051129     c                   else
007600051129     c     kar5          chain     fiar531c
007700051129     c                   endif
007800051129     c
007900051129    6c                   if        not %found
008000051124     c                   eval      olv80tcrd='TASSATIVA'
008100051124     c                   eval      olv80tat='S'
008200051123   x6C                   ELSE
008300051123     c* i dati devono essere quelli immessi dalla partenza
008400051123     c                   movel     ar5uni        dar5gen
008500051124     c                   movel     ilv80dcr      alfadcr           8
008600051128     c* Solo se c'e' la "S" altrimenti non è stata variata la cons richiesta
008700051128     c*  ma il record GEN è stato scritto per altri motivi
008800051128    7c                   if        §ar5mdp='S'
008900051128     c
009000051128    8c                   if        §ar5tcr=' ' and  §ar5dcr=alfadcr
009100051123     c
009200051123     c* ci doveva essere anche la "S" di supermercato
009300051128    9c                   if        §ar5tc1='S' or §ar5tc2='S'
009400051124     c                   eval      olv80tcrd='TASSATIVA'
009500051124     c                   eval      olv80tat='S'
009600051128    9c                   endif
009700051128    8c                   endif
009800051128     c
009900051128   x7c                   else
010000051128     c
010100051128     c                   eval      olv80tcrd='TASSATIVA'
010200051128     c                   eval      olv80tat='S'
010300051123    7C                   ENDIF
010400051123    6C                   ENDIF
010500051123    5C                   ENDIF
010600051123    4C                   ENDIF
010700051123    3C                   ENDSL
010800941219     C*
010900941216     C* DATA CONSEGNA RICHIESTA
011000051123     C                   Z-ADD     ilv80dcr      G02INV
011100941216     C                   MOVEL     '3'           G02ERR
011200941216     C                   CALL      'XSRDA8'
011300941216     C                   PARM                    WLBDAT
011400051124     C                   Z-ADD     G02DAT        olv80dcr8
011500051124     c
011600051124     C                   MOVEL     G02DAT        olv80dcr6
011700051124     C                   MOVE      G02DAT        WDUE              2 0
011800051124     C                   MOVE      WDUE          olv80dcr6
011900051123    2C                   ENDIF
012000051123     C* C'E' L'ORA senza data
012100051123     C     ilv80hcr      IFGT      0
012200051123     C     ilv80dcr      andeq     0
012300941219     C                   SELECT
012400051123     C     ilv80tcr      WHENEQ    'P'
012500051124     C                   eval      olv80tcrd='PRIMA DELLE'
012600051123     C     ilv80tcr      WHENEQ    'D'
012700051124     C                   eval      olv80tcrd='DOPO  LE   '
012800941219     C                   OTHER
012900051124     C                   eval      olv80tcrd='  ALLE     '
013000941219     C                   ENDSL
013100051123    2C                   ENDIF
013200051123    1C                   ENDIF
013300051123     c
013400051123     C     Ilv80tla      IFNE      ' '
013500051123     C                   SETON                                        LR
013600051123     C                   ELSE
013700051123     C                   SETON                                        RT
013800051123     C                   ENDIF
013900991206     C**************************************************************************
014000051123     C     *INZSR        BEGSR
014100051123     c
014200051123     c     kAR5          Klist
014300051123     c                   Kfld                    ilv80Aas
014400051123     c                   Kfld                    ilv80Lnp
014500051123     c                   Kfld                    ilv80nrs
014600051123     c                   Kfld                    ilv80nsp
014700051123     c                   Kfld                    kAr5Trd
014800051123     c
014900051123     C* Verifico da che data fare la tassativa
015000051123     C                   CLEAR                   dsbs02
015100051123     c                   clear                   dvpo
015200051123     C                   MOVEL     'L'           T02tla
015300051123     C                   MOVEL     'C'           T02MOD
015400051123     C                   MOVEL     KNSIF         T02SIF
015500051123     C                   MOVEL     'VPO'         T02COD
015600051123     c                   movel(P)  'VPO'         t02ke1
015700051123     C                   CALL      'TIBS02R'
015800051123     C                   PARM                    KPJBA
015900051123     C                   PARM                    dsbs02
016000051123    2C                   IF        T02ERR = *BLANKS
016100051123     C                   MOVEL     T02uni        dvpo
016200051123    2C                   ENDIF
016300051129     c
016400051129     c* Apro file di filiale se non c'e' quello di sede
016500051129     c                   open      fiar501l                             99
016600051129     c                   if        not %open(fiar501l)
016700051129     c                   open      fiar531c
016800051129     c                   endif
016900051124    cc                   endsr
