000100940224     H*--------------------------------------------------------------*
000200071009     Ffnblp45l  iF   E           K DISK
000300170718     Ffnblp01l  iF   E           K DISK    rename(fnblp000:fnblp001)
000400071009     Ffnlbl01l  iF   E           K DISK
000500170718     Fazorg01l  if   E           K DISK
000600071009     FSYSPRT    O    F  132        PRINTER
000700060928
000800170718     D* DS PER TRUL90R - RICHIESTA STAMPANTI
000900170718     D TRUL90DS      E DS
001000170718     D KPJBA         E DS
001100170718     D DBLP          E DS                  INZ
001200170718     D DTASV         E DS                  INZ
001300170718     D og143         E DS                  INZ
001400900131     C*---------------------------------------------------------------*
001500071009     c     *entry        plist
001600170718     c                   parm                    kpjba
001700170718
001800170718     C                   CLEAR                   trul90DS
001900170718     C                   MOVEL     'S'           d90RSE
002000170718     C                   MOVEL     'S'           d90RSB
002100170718     C                   CALL      'TRUL90R'
002200170718     C                   PARM                    KPJBA
002300170718     C                   PARM                    trul90DS
002400170718      *
002500170718      * F3 - FINE
002600170718     C     d90F3         IFEQ      '1'
002700170718     C                   GOTO      FINE
002800170718     C                   ENDIF
002900071009     c*
003000071009     C     Karg          KLIST
003100071009     C                   KFLD                    blPAAS
003200071009     C                   KFLD                    bLPLNP
003300071009     C                   KFLD                    bLPNRS
003400071009     C                   KFLD                    bLPNSP
003500071009     c*
003600071009     c     *loval        setll     fnblp45l
003700071009    1c                   do        *hival
003800071009     c                   read      fnblp45l
003900071009    2c                   if        %eof(fnblp45l)
004000071009     c                   leave
004100071009    2c                   endif
004200071009     c* scarto se figlia
004300071009     c     karg          setll     fnlbl01l
004400071009     c                   if        %equal(fnlbl01l)
004500071009     c                   iter
004600071009     c                   endif
004700071009     c*
004800170718     c* Scarto lna export e recupero
004900170718     c                   if        blpctm ='4 ' or blpctm='4F'
005000170718     c                             or blpnrs>0 or blplnp<>190
005100170718     c                             or blpdse>0 or blplna<>72
005200170718     c                   iter
005300170718     c                   endif
005400170718
005500170718     c     blplna        chain     azorg01l
005600170718     c                   if        %found(azorg01l)
005700170718     c                   eval      og143=orgde3
005800170718     c                   else
005900170718     c                   clear                   og143
006000170718     c                   endif
006100170718     c                   if        §ogntw='EEX' or §ogntw='DPD' or
006200170718     c                             §ogntw='FED'
006300170718     c                   iter
006400170718     c                   endif
006500170718
006600170718     c* data immissione < della data nuovo capario --> elaboro
006700170718    2c                   if        blpdim < 20170717
006800071009     c*
006900170718     c* richiamo fnls01r
007000170718     c                   movel     blplna        savlna            3 0
007100170718     C                   MOVEL     BLPAAS        §LPAAS
007200170718     C                   MOVEL     BLPLNP        §LPLNP
007300170718     C                   MOVEL     BLPNRS        §LPNRS
007400170718     C                   MOVEL     BLPNSP        §LPNSP
007500170718     C                   MOVEL     'N'           §LPFPR
007600170718     C*                  MOVEL     PARAM4        KPJBU
007700170718     C                   CALL      'FNLS01R'
007800170718     C                   PARM                    KPJBA
007900170718     C                   PARM                    DBLP
008000170718     C                   PARM                    DTASV
008100170718     C                   PARM                    trul90ds
008200170718     C                   PARM                    blptfp
008300170718     c
008400170718    3c                   if        §lpfpr>'0'
008500170718     c                   except    err
008600170718     c                   add       1             errori            7 0
008700170718   x3c                   else
008800170718     c     karg          chain     fnblp01l
008900170718    4c                   if        %found(fnblp01l) and
009000170718     c                             savlna<>blplna
009100170718     c                   except    bol
009200170718     c                   add       1             aggio             7 0
009300170718    4c                   endif
009400170718    3c                   endif
009500170718
009600170718     c                   add       1             bolle             7 0
009700170718     C*
009800071009    2c                   endif
009900071009    1c                   enddo
010000071009     c*
010100071009      *
010200170718     c                   except    tot
010300170718     c     fine          tag
010400071009     C                   SETON                                        LR
010500170718     Osysprt    e            err         1
010600071009     O                       blpaas           +   2
010700071009     O                       blplnp           +   2
010800071009     O                       blpnrs        z  +   2
010900071009     O                       blpnsp        z  +   2
011000071009     o                       blpdim           +   2
011100170718     o                                        +   2 'ERRORE'
011200170718     Osysprt    e            bol         1
011300170718     O                       blpaas           +   2
011400170718     O                       blplnp           +   2
011500170718     O                       blpnrs        z  +   2
011600170718     O                       blpnsp        z  +   2
011700170718     o                       blpdim           +   2
011800170718     o                                        +   2 'OLD lna'
011900170718     o                       savlna           +   2
012000170718     o                                        +   2 'NEW lna'
012100170718     o                       blplna           +   2
012200170718     Osysprt    e            tot         1
012300170718     o                                              'Bolle elaborate'
012400170718     O                       bolle         2  +   2
012500170718     o                                        +   2 'aggiorante'
012600170718     O                       aggio         2  +   2
012700170718     o                                        +   2 'errate'
012800170718     O                       errori        2  +   2
