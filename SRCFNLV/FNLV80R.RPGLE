000100030718     H DECEDIT('0,') DATEDIT(*DMY.)  OPTION(*NODEBUGIO)
000200051123     H* FNLV80R *----------------------------------------------------*
000300051123     H* - decodifica consegna richiesta
000400000000     H*--------------------------------------------------------------*
000500051129     fFiar501l  if   e           k Disk    usropn
000600051129     fFiar531c  if   e           k Disk    usropn
000700051129     F                                     RENAME(fiar5000:fiar5tas)
000800030722     F**
000900170203     d*dVpo          e ds
001000051123     D DAR5GEN       E DS
001100051123     D* DS PER TIBS02R - GESTIONE TNTBE00F
001200170203     D*DSBS02        E DS                  EXTNAME(TIBS02DS)
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
002900070329     d
003000070329     d ILV80NWPROC     s              1
003100900518     C***************************************************************
003200000000     C     *ENTRY        PLIST
003300000000     C                   PARM                    KPJBA
003400051123     C                   PARM                    fnlv80ds
003500070329     c* se ILN80NWPROC='S' --> significa nuova procedura TASSATIVA che
003600070329     c*                        considera anche gli Appuntamenti
003601170203     c* Non utilizzato ma lo lascio visto che i chiamanti lo passano
003700070329     C                   PARM                    ilv80nwproc
003800940223     C*---------------------------------------------------------------*
003900051123     C* TIPO LANCIO, ilv80TLA "C" -           CHIUSO CON LR
004000051123     C* TIPO LANCIO, ilv80TLA "L" - ELABORO E CHIUDO CON LR
004100051123     C* TIPO LANCIO, ilv80TLA " " - ELABORO E CHIUDO IN RETRN
004200941216     C*
004300051123    1C     ilv80TLA      IFNE      'C'
004400051123     c                   clear                   olv80err
004500051123     c                   clear                   olv80msg
004600051124     c                   clear                   olv80tcrd
004700051124     c                   clear                   olv80dcr8
004800051124     c                   clear                   olv80dcr6
004900051123     c
005000051123     C* SE NON PASSATA DATA SPEDIZIONE CALCOLO UDATE
005100051123     C                   IF        ILV80MGS=0
005200051123     C                   TIME                    W0140            14 0
005300051123     C* UDATE IN GGMMAAAA
005400051123     C                   MOVE      W0140         WDTGIO            8 0
005500051123     c     *eur          Move      wdtgio        Dataiso
005600051123     c                   Move      Dataiso       DATEU             8 0
005700051123     c                   endif
005800051123     c
005900051123     c
006000941219     C* C'E' LA DATA
006100051123    2C     ilv80dcr      IFGT      0
006200941219     C*
006300051123    3C                   SELECT
006400051123     C     ilv80tcr      WHENEQ    'P'
006500051124     C                   eval      olv80tcrd='PRIMA DEL'
006600051123     C     ilv80tcr      WHENEQ    'D'
006700051124     C                   eval      olv80tcrd='DOPO  IL '
006800941216     C                   OTHER
006900051124     C                   eval      olv80tcrd='      IL '
007000051123     C* VEDO SE TASSATIVA
007100070402     C
007200070402     C                   CLEAR                   CTRTAS            1
007300070402    4C                   if        (ILV80TC1='S' OR ILV80TC2='S')
007400070402     C                   EVAL      CTRTAS='S'
007500070402     C                   ENDIF
007600070329     c
007700170203     c****               IF        (%parms=3 and ilv80nwproc='S') and
007800170203    4C                   if        (ILV80TC1='A' OR ILV80TC2='A')
007900170203     C****               EVAL      CTRTAS='S'
007901170203     c                   eval      olv80tcrd='TASSATIVA'
007902170203     c                   eval      olv80tat='S'
008000070402     C                   ENDIF
008100070329     c
008200070402     c                   if        CTRTAS='S'
008300170203     c****               movel     ilv80aas      datasped
008400170203     c****               move      ilv80mgs      datasped
008500051123     c* solo se già partita la procedura
008600170203    5c***                if        (ilv80mgs>0 and datasped>=§vpodcrta) or
008700170203     c***                          (ilv80mgs=0 and dateu>=§vpodcrta)
008800051123     C* solo se immessa in partenza
008900051129     c                   if        %open(fiar501l)
009000051123     c     kar5          chain     fiar501l
009100051129     c                   else
009200051129     c     kar5          chain     fiar531c
009300051129     c                   endif
009400051129     c
009500051129    6c                   if        not %found
009600051124     c                   eval      olv80tcrd='TASSATIVA'
009700051124     c                   eval      olv80tat='S'
009800051123   x6C                   ELSE
009900051123     c* i dati devono essere quelli immessi dalla partenza
010000051123     c                   movel     ar5uni        dar5gen
010100051124     c                   movel     ilv80dcr      alfadcr           8
010200051128     c* Solo se c'e' la "S" altrimenti non è stata variata la cons richiesta
010300051128     c*  ma il record GEN è stato scritto per altri motivi
010400051128    7c                   if        §ar5mdp='S'
010500051128     c
010600051128    8c                   if        §ar5tcr=' ' and  §ar5dcr=alfadcr
010700051123     c
010800051123     c* ci doveva essere anche la "S" di supermercato
010900051128    9c                   if        §ar5tc1='S' or §ar5tc2='S'
011000051124     c                   eval      olv80tcrd='TASSATIVA'
011100051124     c                   eval      olv80tat='S'
011200051128    9c                   endif
011300051128    8c                   endif
011400051128     c
011500051128   x7c                   else
011600051128     c
011700051128     c                   eval      olv80tcrd='TASSATIVA'
011800051128     c                   eval      olv80tat='S'
011900051123    7C                   ENDIF
012000051123    6C                   ENDIF
012100170203    5C*****              ENDIF
012200051123    4C                   ENDIF
012300051123    3C                   ENDSL
012400941219     C*
012500941216     C* DATA CONSEGNA RICHIESTA
012600051123     C                   Z-ADD     ilv80dcr      G02INV
012700941216     C                   MOVEL     '3'           G02ERR
012800941216     C                   CALL      'XSRDA8'
012900941216     C                   PARM                    WLBDAT
013000051124     C                   Z-ADD     G02DAT        olv80dcr8
013100051124     c
013200051124     C                   MOVEL     G02DAT        olv80dcr6
013300051124     C                   MOVE      G02DAT        WDUE              2 0
013400051124     C                   MOVE      WDUE          olv80dcr6
013500051123    2C                   ENDIF
013600051123     C* C'E' L'ORA senza data
013700051123     C     ilv80hcr      IFGT      0
013800051123     C     ilv80dcr      andeq     0
013900941219     C                   SELECT
014000051123     C     ilv80tcr      WHENEQ    'P'
014100051124     C                   eval      olv80tcrd='PRIMA DELLE'
014200051123     C     ilv80tcr      WHENEQ    'D'
014300051124     C                   eval      olv80tcrd='DOPO  LE   '
014400941219     C                   OTHER
014500051124     C                   eval      olv80tcrd='  ALLE     '
014600941219     C                   ENDSL
014700051123    2C                   ENDIF
014800051123    1C                   ENDIF
014900051123     c
015000051123     C     Ilv80tla      IFNE      ' '
015100051123     C                   SETON                                        LR
015200051123     C                   ELSE
015300051123     C                   SETON                                        RT
015400051123     C                   ENDIF
015500991206     C**************************************************************************
015600051123     C     *INZSR        BEGSR
015700051123     c
015800051123     c     kAR5          Klist
015900051123     c                   Kfld                    ilv80Aas
016000051123     c                   Kfld                    ilv80Lnp
016100051123     c                   Kfld                    ilv80nrs
016200051123     c                   Kfld                    ilv80nsp
016300051123     c                   Kfld                    kAr5Trd
016400051123     c
016500051123     C* Verifico da che data fare la tassativa
016600170203     C***                CLEAR                   dsbs02
016700170203     c***                clear                   dvpo
016800170203     C***                MOVEL     'L'           T02tla
016900170203     C***                MOVEL     'C'           T02MOD
017000170203     C***                MOVEL     KNSIF         T02SIF
017100170203     C***                MOVEL     'VPO'         T02COD
017200170203     c***                movel(P)  'VPO'         t02ke1
017300170203     C***                CALL      'TIBS02R'
017400170203     C***                PARM                    KPJBA
017500170203     C***                PARM                    dsbs02
017600170203    2C***                IF        T02ERR = *BLANKS
017700170203     C***                MOVEL     T02uni        dvpo
017800170203    2C***                ENDIF
017900051129     c
018000051129     c* Apro file di filiale se non c'e' quello di sede
018100051129     c                   open      fiar501l                             99
018200051129     c                   if        not %open(fiar501l)
018300051129     c                   open      fiar531c
018400051129     c                   endif
018500051124    cc                   endsr
