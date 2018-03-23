000100930527     H DECEDIT('0,') DATEDIT(*DMY.)
000200970902     H* FNLV57R *-----------------------------------------------------*
000300970902     H*     - AGGIORNA TASSAZIONE PADRONCINI
000400930322     H*---------------------------------------------------------------*
000500930322     H* INDICATORI USATI                                              *
000600930322     H*---------------------------------------------------------------*
000700940613     H* 18 - OFF  FILE TASSAZIONE PADRONCINI
000800940613     H* 18 - ON   FILE TASSAZIONE PADRONCINI   S I M U L A Z I O N E
000900941231     H* 30 - DI COMODO
001000941231     H* 31 - DI COMODO
001100930322     H*---------------------------------------------------------------*
001200930322     H* DATA BASE                                                     *
001300930322     H*---------------------------------------------------------------*
001400060126     Ffiftt01L  IF   E           K DISK
001401060126     /* ?Dopo Aver portato in UNI i Padroncini la vista logica 02L è stata
001402060126     /* ?sostituita con la 22L che seleziona solo i records con ATB = *blank
001500060126     F****fiftd02L  UF   E           K DISK
001501060126     Ffiftd22L  UF   E           K DISK
001600011120     F                                     RENAME(fiftd000:fiftd002)
001700040204     Ffiftd05L  UF   E           K DISK
001800011120     Ffifst01L  IF   E           K DISK
001900011120     F                                     RENAME(fiftt000:fifst000)
002000011120     Ffifsd02L  UF   E           K DISK
002100011120     F                                     RENAME(fiftd000:fifsd002)
002200040204     Ffifsd05L  UF   E           K DISK
002300011120     F                                     RENAME(fiftd000:fifsd000)
002400930322     D*------------------
002500930322     D* PARAMETRI ESTERNI
002600930322     D*------------------
002700930322     D KPJBA         E DS
002800930329     D  LIBSYS                92    101
002900940323     D*
003000941231     D* PASSAGGIO DATI A SVALORIZZAZ. DISTINTA PADRONC.  - FNLRE2R -
003100940323     D PARAM3          DS
003200940323     D  PA3PDR                 1      7  0
003300940323     D  PA3TSR                 8      8
003400940323     D  PA3NDC                 9     15  0
003500940323     D  PA3FCT                16     16
003600940323     D  PA3FVD                17     17
003700940323     D  PA3FVT                18     18
003800950113     D  PA3DDC                19     26  0
003900041122     D  PA3sml               256    256
004000940323     D*
004100970902     D DSLV57        E DS                  EXTNAME(FNLV57DS)
004200930322     C*---------------------------------------------------------------*
004300970902     C                   EXSR      DEFVAR
004400970902     C**
004500970902     C* TIPO LANCIO, TLA "C" -           CHIUSO CON LR
004600970902     C* TIPO LANCIO, TLA "L" - ELABORO E CHIUDO CON LR
004700970902     C* TIPO LANCIO, TLA " " - ELABORO E CHIUDO IN RETRN
004800970902    0C     I57TLA        IFNE      'C'
004900970902     C**
005000970902     C                   CLEAR                   VARIAZ
005100970902     C                   CLEAR                   WVOLU
005200970902     C                   CLEAR                   WPESO
005300970902     C                   CLEAR                   WDEST
005400040204     C                   CLEAR                   WADDV
005500040204     C                   CLEAR                   WADDP
005600970902     C* SOLO SE E' VARIATO
005700970902    1C     I57TVL        IFEQ      'C'
005800970902     C     I57VCN        ANDNE     I57VLC
005900970903    1C     I57TVL        OREQ      'F'
006000970903     C     I57VFN        ANDNE     I57VLF
006100940323     C*
006200970902     C**
006300040204     C* WADDV  ===>  VALORE NUOVO DA AGGIornare
006400050111     C     I57NCN        ifeq      I57NCL
006500970902     C                   Z-ADD     I57VCN        WADDV
006600940323     C*
006700970902   X2C                   ELSE
006800970902    3C     I57VCN        IFGE      I57VFN
006900970902     C                   Z-ADD     I57VCN        WADDV
007000940323     C                   ELSE
007100970902     C                   Z-ADD     I57VFN        WADDV
007200970902    3C                   ENDIF
007300970902    2C                   ENDIF
007400040204     c*
007500970902     C                   MOVEL     'S'           VARIAZ            1
007600970902     C                   MOVEL     'S'           WVOLU             1
007700970902    1C                   ENDIF
007800970902     C* P E S O
007900040204     C* SOLO SE E' VARIATO
008000040204    1C     I57TVL        IFEQ      'C'
008100040204     C     I57pkc        ANDNE     I57pcn
008200040204    1C     I57TVL        OREQ      'F'
008300040204     C     I57pkf        ANDNE     I57pfn
008400040204     C**
008500040204     C* WADDV  ===>  VALORE NUOVO DA AGGIornare
008600050111     C     I57NpN        ifeq      I57NCL
008700040204     C                   Z-ADD     I57pcn        WADDP
008800040204     C*
008900040204   X2C                   ELSE
009000040204    3C     I57pCN        IFGE      I57pFN
009100040204     C                   Z-ADD     I57pCN        WADDP
009200040204     C                   ELSE
009300040204     C                   Z-ADD     I57pFN        WADDP
009400040204    3C                   ENDIF
009500040204    2C                   ENDIF
009600040204     c*
009700970902     C                   MOVEL     'S'           VARIAZ            1
009800970902     C                   MOVEL     'S'           WPESO             1
009900970902    2C                   ENDIF
010000970902     C* D E S T I N A T A R I O
010100970902    1C     I57TBO        IFEQ      'A'
010200041008    1C     I57TBO        oreq      ' '
010300041008     C     I57RSD        ifne      *BLANKS
010400970902     C     I57CAD        ANDNE     *BLANKS
010500970902     C     I57FIN        ANDNE     *BLANKS
010600970902     C     I57RDN        ANDNE     *BLANKS
010700970902     C     I57CDN        ANDNE     *BLANKS
010800970902     C     I57FDN        ANDNE     *BLANKS
010900970902     C**
011000970902    2C     I57RSD        IFNE      I57RDN
011100970902     C     I57CAD        ORNE      I57CDN
011200970902     C     I57FIN        ORNE      I57FDN
011300970902     C                   MOVEL     'S'           VARIAZ
011400970902     C                   MOVEL     'S'           WDEST             1
011500970902    2C                   ENDIF
011600970902    1C                   ENDIF
011700041008    1C                   ENDIF
011800940323     C*
011900970902    1C     VARIAZ        IFEQ      'S'
012000970902     C* 18 ON  - FILE TASSAZIONE PADRONCINI   S I M U L A Z I O N E
012100970902     C                   SETOFF                                       18
012200940613     C****
012300940613     C** ESEGUO L'AGGIORNAMENTO DEI FILE PADRONCINI 2 VOLTE:
012400011120     C**  - LA PRIMA   VOLTA PER I FILE fiftt00F/fiftd00F
012500040204     C**  - LA SECONDA VOLTA PER I FILE fifst00F/fifsd00F - SIMULAZIONE
012600940613     C****
012700970902     C* SE TIPO BOLLA = A DEVO AGGIORNARE I RECORD CONSEGNE
012800970902     C*  ALTRIMENTI IL RECORD RITIRI
012900040204     C****
013000970902    3C     I57TBO        IFEQ      'P'
013100041008    3C     I57TBO        oreq      ' '
013200041008     c
013300970902     C* PREPARO LA KEY PER CHAINARE
013400970902     C                   EXSR      PRPKEY
013500040204     c                   endif
013600940323     C*
013700040204    3C                   DO        2
013800041008     C** R i t i r i
013900040204    4C     I57TBO        IFEQ      'P'
014000041008     C     I57TBO        oreq      ' '
014100041008     c
014200041008     c                   if        i57tbo='P'
014300041008     C                   Z-ADD     I57PDR        KPDR
014400041008     c                   else
014500041008     C                   Z-ADD     I57Prt        KPDR
014600041008     c                   endif
014700040204     C****   F N F T T 0 0 F   ****
014800040204     C  n18KFTT          CHAIN     fiftt000                           30
014900040204     C   18KFTT          CHAIN     fifst000                           30
015000940614     C*
015100940323     C* SE GIA' CONFERMATA NON AGGIORNO NIENTE
015200040204    5C  N30FTTFVL        IFNE      'C'
015300940323     C*
015400950105     C****   F N F T D 0 0 F   ****
015500040204     C  N18KFTD5         CHAIN     fiftd000                           30
015600040204     C   18KFTD5         CHAIN     fifsd000                           30
015700940323     C*
015800040209     c                   if        not *in30
015900940323     C* VOLUME
016000970902     C     WVOLU         IFEQ      'S'
016100040204     C                   z-add     WADDV         FTDVLU
016200970902     C                   ENDIF
016300970902     C* PESO
016400970902     C     WPESO         IFEQ      'S'
016500040204     C                   z-add     waddp         FTDPKL
016600970902     C                   ENDIF
016700940323     C*
016800040204     C  N18              UPDATE    fiftd000
016900040204     C   18              UPDATE    fifsd000
017000970902     C                   EXSR      SVAFTT
017100040204    5C                   ENDIF
017200040209    5C                   ENDIF
017300940614     C*
017400041008    4C                   endif
017500041008     C*
017600041008     C* C O N S E G N E
017700041008    4C     I57TBO        IFEQ      'A'
017800041008     C     I57TBO        oreq      ' '
017900041008     c
018000041008     c                   if        i57tbo='A'
018100041008     C                   Z-ADD     I57PDR        KPDR
018200041008     c                   else
018300041008     C                   Z-ADD     I57Pco        KPDR
018400041008     c                   endif
018500041008     C*
018600011120     C  N18KFTDC         SETLL     fiftd002
018700011120     C   18KFTDC         SETLL     fifsd002
018800011120     C  N18KFTDC         READE     fiftd002                               37
018900011120     C   18KFTDC         READE     fifsd002                               37
019000970902     C*
019100040204    5C     *IN37         DOWEQ     *OFF
019200970902     C                   MOVEL     FTDTSR        WTSR
019300970902     C                   MOVEL     FTDNDC        WNDC
019400970902     C                   MOVEL     FTDDDC        WDDC
019500970902     C*
019600011120     C  N18KFTT          CHAIN     fiftt000                           37
019700011120     C   18KFTT          CHAIN     fifst000                           37
019800970902     C*
019900970902     C* SE GIA' CONFERMATA NON AGGIORNO NIENTE
020000040204    6C  N37FTTFVL        IFNE      'C'
020100970902     C* DESTINATARIO
020200040204    7C     WDEST         IFEQ      'S'
020300970902     C                   MOVEL     I57RDN        FTDRSC
020400970902     C                   MOVEL     I57CDN        FTDCAP
020500970902     C                   MOVEL     I57FDN        FTDFIN
020600040204    7C                   ENDIF
020700970902     C*
020800970902     C* SE SI TRATTA DI CONSEGNA PARZIALE, IMPOSTO IL VOLUME UNITARIO
020900040204    7C     WVOLU         IFEQ      'S'
021000040204    8C     I57NCL        IFEQ      FTDNCL
021100970902     C                   Z-ADD     WADDV         FTDVLU
021200970902     C*
021300040204   X8C                   ELSE
021400970902     C     WADDV         DIV       I57NCL        UNIVOL            9 5
021500970902     C     UNIVOL        MULT(H)   FTDNCL        FTDVLU
021600040204    8C                   ENDIF
021700040204    7C                   ENDIF
021800970902     C** PESO
021900040204    7C     WPESO         IFEQ      'S'
022000040204    8C     I57NCL        IFEQ      FTDNCL
022100040204     C                   Z-ADD     waddp         FTDPKL
022200970902     C*
022300040204   X8C                   ELSE
022400040204     C     waddp         DIV       I57NCL        UNIPKG            9 3
022500970902     C     UNIPKG        MULT(H)   FTDNCL        FTDPKL
022600040204    8C                   ENDIF
022700040204    7C                   ENDIF
022800970902     C*
022900040204     C  N18              UPDATE    fiftd002
023000040204     C   18              UPDATE    fifsd002
023100970902     C*
023200970902     C* SE VALORIZZATA SVALORIZZO
023300970902     C                   EXSR      SVAFTT
023400040204    6C                   ENDIF
023500970902     C*
023600011120     C  N18KFTDC         READE     fiftd002                               37
023700011120     C   18KFTDC         READE     fifsd002                               37
023800040204    5C                   ENDDO
023900040204    4C                   ENDIF
024000970902     C**
024100940614     C* 18 ON  - FILE TASSAZIONE PADRONCINI   S I M U L A Z I O N E
024200940614     C                   SETON                                        18
024300040204    3C                   ENDDO
024400940614     C*
024500940323    1C                   ENDIF
024600970902    0C                   ENDIF
024700940323     C*
024800970902     C     I57TLA        IFNE      ' '
024900970902     C                   SETON                                        LR
025000970902     C                   ELSE
025100970902     C                   SETON                                        RT
025200970902     C                   ENDIF
025300950105     C*---------------------------------------------------------------*
025400950105     C* ROUTINE DI DEFINIZIONE VARIABILI, KLIST E PLIST               *
025500950105     C*---------------------------------------------------------------*
025600950105     C     DEFVAR        BEGSR
025700950105     C*
025800950105     C     *ENTRY        PLIST
025900950105     C                   PARM                    KPJBA
026000970902     C                   PARM                    DSLV57
026100950105     C*
026200950105     C* ACCESSO FILE TASSAZIONE PADRONCINI
026300040204     C     KFTD5         KLIST
026400970902     C                   KFLD                    KPDR
026500950113     C                   KFLD                    WTSR
026600950105     C                   KFLD                    WNDC
026700950113     C                   KFLD                    WDDC
026800040204     C                   KFLD                    I57AAS
026900040204     C                   KFLD                    I57LNP
027000040204     C                   KFLD                    I57NRS
027100040204     C                   KFLD                    I57NSP
027200950105     C     KFTT          KLIST
027300970902     C                   KFLD                    KPDR
027400950105     C                   KFLD                    WTSR
027500950105     C                   KFLD                    WNDC
027600950113     C                   KFLD                    WDDC
027700970902     C     KFTDC         KLIST
027800970902     C                   KFLD                    I57AAS
027900970902     C                   KFLD                    I57LNP
028000970902     C                   KFLD                    I57NRS
028100970902     C                   KFLD                    I57NSP
028200950105     C*
028300950105     C* DEFINIZIONE CAMPI
028400970902     C     *LIKE         DEFINE    FTDPDR        KPDR
028500970902     C     *LIKE         DEFINE    FTDKSC        WKSC
028600950113     C     *LIKE         DEFINE    FTDTSR        WTSR
028700950105     C     *LIKE         DEFINE    FTDNDC        WNDC
028800950113     C     *LIKE         DEFINE    FTDDDC        WDDC
028900970902     C     *LIKE         DEFINE    I57VLF        WSOTV
029000970902     C     *LIKE         DEFINE    I57VLF        WADDV
029100040204     C     *LIKE         DEFINE    I57pkf        WADDP
029200950105     C*
029300950105     C                   ENDSR
029400970902     C*---------------------------------------------------------------*
029500970902     C* PREPARA KEY PER CHAINARE I RITIRI
029600970902     C*---------------------------------------------------------------*
029700970902     C     PRPKEY        BEGSR
029800970902     C* IL NUMERO DISTINTA RITIRI E' COSI' COMPOSTO:
029900970902     C*   - FLAG PRESTAZIONE PADRONCINO: "M-MATTINO"    ---> 1000
030000970902     C*                                  "P-POMERIGGIO" ---> 5000
030100970902     C*   - AL NUMERO TROVATO VIENE ADDIZIONATO IL NUMERO RITIRO
030200970902     C                   CLEAR                   WNDC
030300970902    4C     I57FPP        IFEQ      'M'
030400970902     C                   Z-ADD     1000          WNDC
030500970902     C                   ELSE
030600970902     C                   Z-ADD     5000          WNDC
030700970902    4C                   ENDIF
030800970902     C                   ADD       I57NRT        WNDC
030900970902     C                   Z-ADD     I57DRT        WDDC
031000970902     C**
031100970902     C                   MOVEL     'R'           WTSR
031200970902     C                   ENDSR
031300970902     C*---------------------------------------------------------------*
031400970902     C* SVALORIZZA UNA DISTINTA                                       *
031500970902     C*---------------------------------------------------------------*
031600970902     C     SVAFTT        BEGSR
031700970902     C* SOLO SE VALORIZZATA
031800970902    1C     FTTFVL        IFEQ      'V'
031900970902     C                   MOVEL     FTTPDR        PA3PDR
032000970902     C                   MOVEL     FTTTSR        PA3TSR
032100970902     C                   Z-ADD     FTTNDC        PA3NDC
032200970902     C                   Z-ADD     FTTDDC        PA3DDC
032300970902     C                   MOVEL     'N'           PA3FCT
032400970902     C                   MOVEL     'N'           PA3FVD
032500970902     C                   MOVEL     'N'           PA3FVT
032600041122     c
032700041122     C  n18              MOVEL     ' '           PA3sml
032800041122     C   18              MOVEL     'S'           PA3sml
032900970902     C                   MOVEL     PARAM3        KPJBU
033000011120     C  N18              CALL      'FICNE2R'
033100970902     C                   PARM                    KPJBA
033200011120     C   18              CALL      'FICNE9C'
033300970902     C                   PARM                    KPJBA
033400970902    1C                   ENDIF
033500970902     C                   ENDSR
