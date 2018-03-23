000100970527     H*--------------------------------------------------------------------------------------------*
000200970527     H* Aggiorna un cap dai suoi archivi
000300970527     H*--------------------------------------------------------------------------------------------*
000400970527     H DECEDIT('0,') DATEDIT(*DMY.)
000500970527     F*--------------------------------------------------------------------------------------------*
000600970527     F* Data base
000700970603     F*--------------------------------------------------------------------------------------------*
000800970527     Fazcpc01l  UF   E           K DISK                                         *Cap
000900970527     Fazcpl01l  IF   E           K DISK                                         *Località
001000970527     Fazcpe01l  IF   E           K DISK                                         *Eccezioni
001100970609     d*--------------------------------------------------------------------------------------------*
001200970609     d* Data structure
001300970609     d*--------------------------------------------------------------------------------------------*
001400970527     D*----------------
001500970527     D* Variabili riferite al data base
001600970527     D*----------------
001700970527     D kccver          S                   LIKE(cpcver)                         *lettura azcpc00f
001800970527     D kccnar          S                   LIKE(cpcnar)
001900970527     D kcccap          S                   LIKE(cpccap)
002000970527     D kcever          S                   LIKE(cpever)                         *lettura azcpe00f
002100970527     D kcenar          S                   LIKE(cpenar)
002200970527     D kcecap          S                   LIKE(cpecap)
002300970527     D kclver          S                   LIKE(cplver)                         *lettura azcpl00f
002400970527     D kclnar          S                   LIKE(cplnar)
002500970527     D kclcap          S                   LIKE(cplcap)
002600970527     D*----------------
002700970527     D* Variabili a programma
002800970527     D*----------------
002900970609     D wese            S              1                                         *esistono eccezioni
003000970609     D wesl            S              1                                         *località
003100970609     d*----------------
003200970609     d* Ds
003300970609     d*----------------
003400970609     d dssi81        E DS                  EXTNAME(tisi81ds)                    *ds di procedura
003500970609     D kpjba         E DS                                                       *Architettura
003600970527     C*--------------------------------------------------------------------------------------------*
003700970527     C* Main lines
003800970527     C*--------------------------------------------------------------------------------------------*
003900970527     C*
004000970527     C* controllo eccezioni-> legge le eccezioni del cap
004100970527if  1c                   IF        d81chk='E'
004200970527     C                   EXSR      letcpe
004300970527e   1c                   ENDIF
004400970527     C*
004500970527     C* controllo località-> legge le località del cap
004600970527if  1c                   IF        d81chk='L'
004700970527     C                   EXSR      letcpl
004800970527e   1c                   ENDIF
004900970527     C*
005000970527     C* aggiorna il cap
005100970527     C                   EXSR      aggcpc
005200970527     C*
005300970527     C                   SETON                                        lr
005400970527     C*--------------------------------------------------------------------------------------------*
005500970527     C* letcpe - legge le eccezioni del cap
005600970527     C*--------------------------------------------------------------------------------------------*
005700970527     C     letcpe        BEGSR
005800970527     c*
005900970527     c* imposta a NO esistenza eccezioni
006000970609     c                   MOVEL     'N'           wese
006100970527     c*
006200970527     c                   Z-ADD     d81ver        kcever
006300970527     c                   MOVEL     d81nar        kcenar
006400970527     c                   MOVEL     d81cap        kcecap
006500970527     c     keycpe        SETLL     azcpe01l
006600970527     c     keycpe        READE     azcpe01l                               99
006700970527do  1c                   DOW       NOT *in99
006800970527if  2c                   IF        cpeatb=*blanks                               *no annullate
006900970609     c                   MOVEL     'S'           wese                           *esistono eccezioni
007000970527e   2c                   ENDIF
007100970527     c     keycpe        READE     azcpe01l                               99
007200970527e   1c                   ENDDO
007300970527     c*
007400970527     C                   ENDSR
007500970527     C*--------------------------------------------------------------------------------------------*
007600970527     C* letcpl - legge le località del cap
007700970527     C*--------------------------------------------------------------------------------------------*
007800970527     C     letcpl        BEGSR
007900970527     c*
008000970527     c* imposta a NO esistenza località gestite
008100970609     c                   MOVEL     'N'           wesl
008200970527     c*
008300970527     c                   Z-ADD     d81ver        kclver
008400970527     c                   MOVEL     d81nar        kclnar
008500970527     c                   MOVEL     d81cap        kclcap
008600970527     c     keycpl        SETLL     azcpl01l
008700970527     c     keycpl        READE     azcpl01l                               99
008800970527do  1c                   DOW       NOT *in99
008900970527if  2c                   IF        cplatb=*blanks                               *no annullate
009000970527if  3c                   IF        cplliv='L'                                   *gestite
009100970609     c                   MOVEL     'S'           wesl                           *esistono località
009200970527e   3c                   ENDIF
009300970527e   2c                   ENDIF
009400970527     c     keycpl        READE     azcpl01l                               99
009500970527e   1c                   ENDDO
009600970527     c*
009700970527     C                   ENDSR
009800970527     C*--------------------------------------------------------------------------------------------*
009900970527     C* aggcpc - Aggiorna il cap
010000970527     C*--------------------------------------------------------------------------------------------*
010100970527     C     aggcpc        BEGSR
010200970527     c*
010300970527     c                   Z-ADD     d81ver        kccver
010400970527     c                   MOVEL     d81nar        kccnar
010500970527     c                   MOVEL     d81cap        kcccap
010600970527     c     keycpc        CHAIN     azcpc01l                           99
010700970527     c*
010800970527     c* aggiorna per eccezioni
010900970527if  1c                   IF        d81chk='E'
011000970609     c                   MOVEL     wese          cpcese
011100970527     c                   MOVEL     *blanks       cpcftr
011200970527     c                   EXCEPT    updcpce
011300970527e   1c                   ENDIF
011400970527     c*
011500970527     c* aggiorna per località
011600970527if  1c                   IF        d81chk='L'
011700970609     c                   MOVEL     wesl          cpcesl
011800970527     c                   MOVEL     *blanks       cpcftr
011900970527     c                   EXCEPT    updcpcl
012000970527e   1c                   ENDIF
012100970527     c*
012200970527     C                   ENDSR
012300970527     C*--------------------------------------------------------------------------------------------*
012400970527     C* *inzsr - Operazioni iniziali
012500970527     C*--------------------------------------------------------------------------------------------*
012600970527     C     *inzsr        BEGSR
012700970527     C*---
012800970527     C* ricevimento parametri
012900970527     C*---
013000970527     C     *ENTRY        PLIST
013100970527     C                   PARM                    dssi81
013200970527     C*---
013300970527     C* chiavi di lettura
013400970527     C*---
013500970527     C* di azcpc01l - completa
013600970527     C     keycpc        KLIST
013700970527     C                   KFLD                    kccver                         *versione
013800970527     C                   KFLD                    kccnar                         *nazione
013900970527     C                   KFLD                    kcccap                         *cap
014000970527     C* di azcpe01l - parziale
014100970527     C     keycpe        KLIST
014200970527     C                   KFLD                    kcever                         *versione
014300970527     C                   KFLD                    kcenar                         *nazione
014400970527     C                   KFLD                    kcecap                         *cap
014500970527     C* di azcpl01l - parziale
014600970527     C     keycpl        KLIST
014700970527     C                   KFLD                    kclver                         *versione
014800970527     C                   KFLD                    kclnar                         *nazione
014900970527     C                   KFLD                    kclcap                         *cap
015000970527     C*
015100970527     C                   ENDSR
015200970527     O*--------------------------------------------------------------------------------------------*
015300970527     O* Aggiornamenti
015400970527     O*--------------------------------------------------------------------------------------------*
015500970527     Oazcpc000  E            updcpce
015600970527     O                       cpcese
015700970527     O                       cpcftr
015800970527     O*----------
015900970527     Oazcpc000  E            updcpcl
016000970527     O                       cpcesl
016100970527     O                       cpcftr
