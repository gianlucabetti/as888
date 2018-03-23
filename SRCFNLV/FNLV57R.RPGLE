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
001500060126     /* ?Dopo Aver portato in UNI i Padroncini la vista logica 02L è stata
001600060126     /* ?sostituita con la 22L che seleziona solo i records con ATB = *blank
001700060126     F****fiftd02L  UF   E           K DISK
001800060126     Ffiftd22L  UF   E           K DISK
001900011120     F                                     RENAME(fiftd000:fiftd002)
002000040204     Ffiftd05L  UF   E           K DISK
002100011120     Ffifst01L  IF   E           K DISK
002200011120     F                                     RENAME(fiftt000:fifst000)
002300011120     Ffifsd02L  UF   E           K DISK
002400011120     F                                     RENAME(fiftd000:fifsd002)
002500040204     Ffifsd05L  UF   E           K DISK
002600011120     F                                     RENAME(fiftd000:fifsd000)
002700930322     D*------------------
002800930322     D* PARAMETRI ESTERNI
002900930322     D*------------------
003000930322     D KPJBA         E DS
003100930329     D  LIBSYS                92    101
003200940323     D*
003300941231     D* PASSAGGIO DATI A SVALORIZZAZ. DISTINTA PADRONC.  - FNLRE2R -
003400940323     D PARAM3          DS
003500940323     D  PA3PDR                 1      7  0
003600940323     D  PA3TSR                 8      8
003700940323     D  PA3NDC                 9     15  0
003800940323     D  PA3FCT                16     16
003900940323     D  PA3FVD                17     17
004000940323     D  PA3FVT                18     18
004100950113     D  PA3DDC                19     26  0
004200041122     D  PA3sml               256    256
004300940323     D*
004400970902     D DSLV57        E DS                  EXTNAME(FNLV57DS)
004500930322     C*---------------------------------------------------------------*
004600970902     C                   EXSR      DEFVAR
004700070705     c                   setoff                                       99
004800970902     C**
004900970902     C* TIPO LANCIO, TLA "C" -           CHIUSO CON LR
005000970902     C* TIPO LANCIO, TLA "L" - ELABORO E CHIUDO CON LR
005100970902     C* TIPO LANCIO, TLA " " - ELABORO E CHIUDO IN RETRN
005200970902    0C     I57TLA        IFNE      'C'
005300970902     C**
005400970902     C                   CLEAR                   VARIAZ
005500970902     C                   CLEAR                   WVOLU
005600970902     C                   CLEAR                   WPESO
005700970902     C                   CLEAR                   WDEST
005800040204     C                   CLEAR                   WADDV
005900040204     C                   CLEAR                   WADDP
006000970902     C* SOLO SE E' VARIATO
006100970902    1C     I57TVL        IFEQ      'C'
006200970902     C     I57VCN        ANDNE     I57VLC
006300970903    1C     I57TVL        OREQ      'F'
006400970903     C     I57VFN        ANDNE     I57VLF
006500940323     C*
006600970902     C**
006700040204     C* WADDV  ===>  VALORE NUOVO DA AGGIornare
006800050111     C     I57NCN        ifeq      I57NCL
006900970902     C                   Z-ADD     I57VCN        WADDV
007000940323     C*
007100970902   X2C                   ELSE
007200970902    3C     I57VCN        IFGE      I57VFN
007300970902     C                   Z-ADD     I57VCN        WADDV
007400940323     C                   ELSE
007500970902     C                   Z-ADD     I57VFN        WADDV
007600970902    3C                   ENDIF
007700970902    2C                   ENDIF
007800040204     c*
007900970902     C                   MOVEL     'S'           VARIAZ            1
008000970902     C                   MOVEL     'S'           WVOLU             1
008100970902    1C                   ENDIF
008200970902     C* P E S O
008300040204     C* SOLO SE E' VARIATO
008400040204    1C     I57TVL        IFEQ      'C'
008500040204     C     I57pkc        ANDNE     I57pcn
008600040204    1C     I57TVL        OREQ      'F'
008700040204     C     I57pkf        ANDNE     I57pfn
008800040204     C**
008900040204     C* WADDV  ===>  VALORE NUOVO DA AGGIornare
009000050111     C     I57NpN        ifeq      I57NCL
009100040204     C                   Z-ADD     I57pcn        WADDP
009200040204     C*
009300040204   X2C                   ELSE
009400040204    3C     I57pCN        IFGE      I57pFN
009500040204     C                   Z-ADD     I57pCN        WADDP
009600040204     C                   ELSE
009700040204     C                   Z-ADD     I57pFN        WADDP
009800040204    3C                   ENDIF
009900040204    2C                   ENDIF
010000040204     c*
010100970902     C                   MOVEL     'S'           VARIAZ            1
010200970902     C                   MOVEL     'S'           WPESO             1
010300970902    2C                   ENDIF
010400970902     C* D E S T I N A T A R I O
010500970902    1C     I57TBO        IFEQ      'A'
010600041008    1C     I57TBO        oreq      ' '
010700041008     C     I57RSD        ifne      *BLANKS
010800970902     C     I57CAD        ANDNE     *BLANKS
010900970902     C     I57FIN        ANDNE     *BLANKS
011000970902     C     I57RDN        ANDNE     *BLANKS
011100970902     C     I57CDN        ANDNE     *BLANKS
011200970902     C     I57FDN        ANDNE     *BLANKS
011300970902     C**
011400970902    2C     I57RSD        IFNE      I57RDN
011500970902     C     I57CAD        ORNE      I57CDN
011600970902     C     I57FIN        ORNE      I57FDN
011700970902     C                   MOVEL     'S'           VARIAZ
011800970902     C                   MOVEL     'S'           WDEST             1
011900970902    2C                   ENDIF
012000970902    1C                   ENDIF
012100041008    1C                   ENDIF
012200940323     C*
012300970902    1C     VARIAZ        IFEQ      'S'
012400970902     C* 18 ON  - FILE TASSAZIONE PADRONCINI   S I M U L A Z I O N E
012500970902     C                   SETOFF                                       18
012600940613     C****
012700940613     C** ESEGUO L'AGGIORNAMENTO DEI FILE PADRONCINI 2 VOLTE:
012800011120     C**  - LA PRIMA   VOLTA PER I FILE fiftt00F/fiftd00F
012900040204     C**  - LA SECONDA VOLTA PER I FILE fifst00F/fifsd00F - SIMULAZIONE
013000940613     C****
013100970902     C* SE TIPO BOLLA = A DEVO AGGIORNARE I RECORD CONSEGNE
013200970902     C*  ALTRIMENTI IL RECORD RITIRI
013300040204     C****
013400970902    3C     I57TBO        IFEQ      'P'
013500041008    3C     I57TBO        oreq      ' '
013600041008     c
013700970902     C* PREPARO LA KEY PER CHAINARE
013800970902     C                   EXSR      PRPKEY
013900040204     c                   endif
014000940323     C*
014100040204    3C                   DO        2
014200041008     C** R i t i r i
014300040204    4C     I57TBO        IFEQ      'P'
014400041008     C     I57TBO        oreq      ' '
014500041008     c
014600041008     c                   if        i57tbo='P'
014700041008     C                   Z-ADD     I57PDR        KPDR
014800041008     c                   else
014900041008     C                   Z-ADD     I57Prt        KPDR
015000041008     c                   endif
015100070703     c
015200070705     c                   setoff                                       99
015300040204     C****   F N F T T 0 0 F   ****
015400070703     C  n18KFTT          CHAIN     fiftt000                           30
015500070703     C   18KFTT          CHAIN     fifst000                           30
015600940614     C*
015700940323     C* SE GIA' CONFERMATA NON AGGIORNO NIENTE
015800040204    5C  N30FTTFVL        IFNE      'C'
015900940323     C*
016000950105     C****   F N F T D 0 0 F   ****
016100070703     C  N18KFTD5         CHAIN     fiftd000                           3099
016200070703     C   18KFTD5         CHAIN     fifsd000                           3099
016300940323     C*
016400070703    6c                   if        not *in30 and not *in99
016500940323     C* VOLUME
016600970902     C     WVOLU         IFEQ      'S'
016700040204     C                   z-add     WADDV         FTDVLU
016800970902     C                   ENDIF
016900970902     C* PESO
017000970902     C     WPESO         IFEQ      'S'
017100040204     C                   z-add     waddp         FTDPKL
017200970902     C                   ENDIF
017300940323     C*
017400040204     C  N18              UPDATE    fiftd000
017500040204     C   18              UPDATE    fifsd000
017600970902     C                   EXSR      SVAFTT
017700070703    6C                   ENDIF
017800040209    5C                   ENDIF
017900070703
018000070703     c* Per allocazione restituire errore al chiamante
018100070703    5c                   if        *in99
018200070703     c                   eval      o57err='1'
018300070703     c                   goto      endagg
018400070703    5c                   endif
018500940614     C*
018600041008    4C                   endif
018700041008     C*
018800041008     C* C O N S E G N E
018900041008    4C     I57TBO        IFEQ      'A'
019000041008     C     I57TBO        oreq      ' '
019100041008     c
019200041008     c                   if        i57tbo='A'
019300041008     C                   Z-ADD     I57PDR        KPDR
019400041008     c                   else
019500041008     C                   Z-ADD     I57Pco        KPDR
019600041008     c                   endif
019700041008     C*
019800011120     C  N18KFTDC         SETLL     fiftd002
019900011120     C   18KFTDC         SETLL     fifsd002
020000070703     C  N18KFTDC         READE     fiftd002                             9937
020100070703     C   18KFTDC         READE     fifsd002                             9937
020200970902     C*
020300070703    5C                   DOW       not *in37 and not *in99
020400970902     C                   MOVEL     FTDTSR        WTSR
020500970902     C                   MOVEL     FTDNDC        WNDC
020600970902     C                   MOVEL     FTDDDC        WDDC
020700970902     C*
020800011120     C  N18KFTT          CHAIN     fiftt000                           37
020900011120     C   18KFTT          CHAIN     fifst000                           37
021000970902     C*
021100970902     C* SE GIA' CONFERMATA NON AGGIORNO NIENTE
021200040204    6C  N37FTTFVL        IFNE      'C'
021300970902     C* DESTINATARIO
021400040204    7C     WDEST         IFEQ      'S'
021500970902     C                   MOVEL     I57RDN        FTDRSC
021600970902     C                   MOVEL     I57CDN        FTDCAP
021700970902     C                   MOVEL     I57FDN        FTDFIN
021800040204    7C                   ENDIF
021900970902     C*
022000970902     C* SE SI TRATTA DI CONSEGNA PARZIALE, IMPOSTO IL VOLUME UNITARIO
022100040204    7C     WVOLU         IFEQ      'S'
022200040204    8C     I57NCL        IFEQ      FTDNCL
022300970902     C                   Z-ADD     WADDV         FTDVLU
022400970902     C*
022500040204   X8C                   ELSE
022600970902     C     WADDV         DIV       I57NCL        UNIVOL            9 5
022700970902     C     UNIVOL        MULT(H)   FTDNCL        FTDVLU
022800040204    8C                   ENDIF
022900040204    7C                   ENDIF
023000970902     C** PESO
023100040204    7C     WPESO         IFEQ      'S'
023200040204    8C     I57NCL        IFEQ      FTDNCL
023300040204     C                   Z-ADD     waddp         FTDPKL
023400970902     C*
023500040204   X8C                   ELSE
023600040204     C     waddp         DIV       I57NCL        UNIPKG            9 3
023700970902     C     UNIPKG        MULT(H)   FTDNCL        FTDPKL
023800040204    8C                   ENDIF
023900040204    7C                   ENDIF
024000970902     C*
024100040204     C  N18              UPDATE    fiftd002
024200040204     C   18              UPDATE    fifsd002
024300970902     C*
024400970902     C* SE VALORIZZATA SVALORIZZO
024500970902     C                   EXSR      SVAFTT
024600040204    6C                   ENDIF
024700970902     C*
024800070703     C  N18KFTDC         READE     fiftd002                             9937
024900070703     C   18KFTDC         READE     fifsd002                             9937
025000040204    5C                   ENDDO
025100070703     c
025200070703     c* Per allocazione passo errore al chiamante
025300070703    5c                   if        *in99
025400070703     c                   eval      o57err='1'
025500070703     c                   goto      endagg
025600070703    5c                   endif
025700040204    4C                   ENDIF
025800970902     C**
025900940614     C* 18 ON  - FILE TASSAZIONE PADRONCINI   S I M U L A Z I O N E
026000940614     C                   SETON                                        18
026100040204    3C                   ENDDO
026200940614     C*
026300940323    1C                   ENDIF
026400970902    0C                   ENDIF
026500940323     C*
026600070703     c     endagg        tag
026700070703     c
026800970902     C     I57TLA        IFNE      ' '
026900970902     C                   SETON                                        LR
027000970902     C                   ELSE
027100970902     C                   SETON                                        RT
027200970902     C                   ENDIF
027300950105     C*---------------------------------------------------------------*
027400950105     C* ROUTINE DI DEFINIZIONE VARIABILI, KLIST E PLIST               *
027500950105     C*---------------------------------------------------------------*
027600950105     C     DEFVAR        BEGSR
027700950105     C*
027800950105     C     *ENTRY        PLIST
027900950105     C                   PARM                    KPJBA
028000970902     C                   PARM                    DSLV57
028100070703     c                   clear                   o57err
028200950105     C*
028300950105     C* ACCESSO FILE TASSAZIONE PADRONCINI
028400040204     C     KFTD5         KLIST
028500970902     C                   KFLD                    KPDR
028600950113     C                   KFLD                    WTSR
028700950105     C                   KFLD                    WNDC
028800950113     C                   KFLD                    WDDC
028900040204     C                   KFLD                    I57AAS
029000040204     C                   KFLD                    I57LNP
029100040204     C                   KFLD                    I57NRS
029200040204     C                   KFLD                    I57NSP
029300950105     C     KFTT          KLIST
029400970902     C                   KFLD                    KPDR
029500950105     C                   KFLD                    WTSR
029600950105     C                   KFLD                    WNDC
029700950113     C                   KFLD                    WDDC
029800970902     C     KFTDC         KLIST
029900970902     C                   KFLD                    I57AAS
030000970902     C                   KFLD                    I57LNP
030100970902     C                   KFLD                    I57NRS
030200970902     C                   KFLD                    I57NSP
030300950105     C*
030400950105     C* DEFINIZIONE CAMPI
030500970902     C     *LIKE         DEFINE    FTDPDR        KPDR
030600970902     C     *LIKE         DEFINE    FTDKSC        WKSC
030700950113     C     *LIKE         DEFINE    FTDTSR        WTSR
030800950105     C     *LIKE         DEFINE    FTDNDC        WNDC
030900950113     C     *LIKE         DEFINE    FTDDDC        WDDC
031000970902     C     *LIKE         DEFINE    I57VLF        WSOTV
031100970902     C     *LIKE         DEFINE    I57VLF        WADDV
031200040204     C     *LIKE         DEFINE    I57pkf        WADDP
031300950105     C*
031400950105     C                   ENDSR
031500970902     C*---------------------------------------------------------------*
031600970902     C* PREPARA KEY PER CHAINARE I RITIRI
031700970902     C*---------------------------------------------------------------*
031800970902     C     PRPKEY        BEGSR
031900970902     C* IL NUMERO DISTINTA RITIRI E' COSI' COMPOSTO:
032000970902     C*   - FLAG PRESTAZIONE PADRONCINO: "M-MATTINO"    ---> 1000
032100970902     C*                                  "P-POMERIGGIO" ---> 5000
032200970902     C*   - AL NUMERO TROVATO VIENE ADDIZIONATO IL NUMERO RITIRO
032300970902     C                   CLEAR                   WNDC
032400970902    4C     I57FPP        IFEQ      'M'
032500970902     C                   Z-ADD     1000          WNDC
032600970902     C                   ELSE
032700970902     C                   Z-ADD     5000          WNDC
032800970902    4C                   ENDIF
032900970902     C                   ADD       I57NRT        WNDC
033000970902     C                   Z-ADD     I57DRT        WDDC
033100970902     C**
033200970902     C                   MOVEL     'R'           WTSR
033300970902     C                   ENDSR
033400970902     C*---------------------------------------------------------------*
033500970902     C* SVALORIZZA UNA DISTINTA                                       *
033600970902     C*---------------------------------------------------------------*
033700970902     C     SVAFTT        BEGSR
033800970902     C* SOLO SE VALORIZZATA
033900970902    1C     FTTFVL        IFEQ      'V'
034000970902     C                   MOVEL     FTTPDR        PA3PDR
034100970902     C                   MOVEL     FTTTSR        PA3TSR
034200970902     C                   Z-ADD     FTTNDC        PA3NDC
034300970902     C                   Z-ADD     FTTDDC        PA3DDC
034400970902     C                   MOVEL     'N'           PA3FCT
034500970902     C                   MOVEL     'N'           PA3FVD
034600970902     C                   MOVEL     'N'           PA3FVT
034700041122     c
034800041122     C  n18              MOVEL     ' '           PA3sml
034900041122     C   18              MOVEL     'S'           PA3sml
035000970902     C                   MOVEL     PARAM3        KPJBU
035100011120     C  N18              CALL      'FICNE2R'
035200970902     C                   PARM                    KPJBA
035300011120     C   18              CALL      'FICNE9C'
035400970902     C                   PARM                    KPJBA
035500970902    1C                   ENDIF
035600970902     C                   ENDSR
