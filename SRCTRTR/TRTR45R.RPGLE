000100020716     H DECEDIT('0,') DATEDIT(*DMY.)
000200000000     H*------------------------------------------------------------------------*
000300000000     H*                    - RICEZIONE IN SEDE STOP -                          *
000400000000     H*------------------------------------------------------------------------*
000500000000     H*------------------------------------------------------------------------*
000600000000     H* DATA BASE
000700000000     H*------------------------------------------------------------------------*
000800970926     FFNSTP00R  UF   E           K DISK    USROPN
000900990622     FTITAS30C  UF   E           K DISK
001000971002     FTABEL00F  IF   E           K DISK
001100000000     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
001200000000     F                                     USROPN
001300000000     D*------------------------------------------------------------------------*
001400000000     D* SCHIERE
001500000000     D*------------------------------------------------------------------------*
001600000000     D*------------------
001700000000     D* COMANDI QCMDEXC
001800000000     D*------------------
001900970926     D CMD1            S             55    DIM(1) CTDATA PERRCD(1)               ALCOBJ
002000970926     D CMD2            S             55    DIM(1) CTDATA PERRCD(1)               OVRDBF
002100000000     D CMD5            S             55    DIM(1) CTDATA PERRCD(1)              *DLCOBJ
002200000000     D*-------------------
002300000000     D* ERRORI DI ELABORAZIONE
002400000000     D*-------------------
002500971002     D ERR             S             60    DIM(5) CTDATA PERRCD(1)
002600000000     D*-------------------
002700000000     D* RIGHE DI STAMPA
002800000000     D*-------------------
002900000000     D STA             S             66    DIM(10) CTDATA PERRCD(1)
003000000000     D*------------------
003100000000     D* ARCHITETTURA
003200000000     D*------------------
003300000000     D KPJBA         E DS
003400000000     D  LIBSYS                92    100
003500000000     D  LEGFIL               483    492
003600000000     D  MBRFIL               493    502
003700950315     D  MBR4                 493    496
003800000000     D*----------------
003900000000     D* DS REPERIMENTO DATI UTENTE
004000000000     D*----------------
004100020716     D**!!! DSFIL         E DS
004200971002     D DSTB          E DS
004300020716
004400020716     d DdatiUte      e ds
004500020716     d AzuteDs       e ds                  ExtName(Azute00f)
004600020716     d Tibs34Ds      e ds                  Inz
004700020716
004800000000     I*------------------------------------------------------------------------*
004900000000     I* INPUT
005000000000     I*------------------------------------------------------------------------*
005100000000     I*------------------
005200000000     I* INDICATORI DI LETTURA
005300000000     I*------------------
005400990622     ITITAS000      01
005500990622     ITITAS010      02
005600990622     ITITASP00      03
005700000000     C*------------------------------------------------------------------------*
005800000000     C* MAIN LINE
005900000000     C*------------------------------------------------------------------------*
006000970926     C* APERTURA MEMBRO DI RICEZIONE
006100970926     C                   EXSR      CAEXR
006200971002     C                   CLEAR                   SAVTBL
006300970926     C*
006400970926     C* APERTURA RIUSCITA
006500970926IF  1C     *IN98         IFEQ      *OFF
006600970926     C                   READ      FNSTP00R                               30
006700970926     C     *IN30         DOWEQ     *OFF
006800970926     C                   EXSR      AGGTAS
006900970926     C                   READ      FNSTP00R                               30
007000970926     C                   ENDDO
007100970926     C* CHIUSURA MEMBRO DI RICEZIONE
007200970926     C                   CLOSE     FNSTP00R
007300970926     C*
007400970926     C                   MOVE      ')'           VAR11
007500970926     C                   MOVEL     MBRFIL        VAR11
007600970926     C                   MOVE      '))'          VAR12
007700970926     C                   MOVEL     MBRFIL        VAR12
007800970926     C* DISALLOCAZIONE
007900970926     C                   MOVE      VAR12         CMD5
008000970926     C                   MOVEL     *BLANKS       COMMAN
008100970926     C                   MOVEA     CMD5(1)       COMMAN
008200970926     C                   CALL      'QCMDEXC'
008300970926     C                   PARM                    COMMAN
008400970926     C                   PARM                    LUNG
008500970926     C*
008600970926     C* CHIUSURA FILE PRINTER
008700970926IF  2C     $OPEN         IFEQ      '1'                                          *MAI APERTO PRTFILE
008800970926     C                   CLOSE     QSYSPRT
008900970926E   2C                   ENDIF
009000970926     C*
009100970926E   1C                   ENDIF
009200000000     C*
009300000000     C                   SETON                                        LR
009400000000     C*------------------------------------------------------------------------*
009500970926     C* AGGTAS - AGGIORNA LE BOLLE E CANCELLA IL RECORD RICEVUTO
009600000000     C*------------------------------------------------------------------------*
009700970926     C     AGGTAS        BEGSR
009800000000     C*
009900950427     C                   SETOFF                                       010203      *LETTURA FORMATI
010000000000     C*
010100000000     C* CONTROLLA ESISTENZA BOLLA
010200970926     C                   Z-ADD     STPAAS        KTAAAS
010300970926     C                   Z-ADD     STPLNP        KTALNP
010400970926     C                   Z-ADD     STPNRS        KTANRS
010500970926     C                   Z-ADD     STPNSP        KTANSP
010600990622     C     KEYTAS        SETLL     TITAS30C                           97
010700990622     C     KEYTAS        READE     TITAS30C                               97
010800970926     C* INESISTENTE
010900970926IF  1C     *IN97         IFEQ      *ON
011000990622     C                   CLEAR                   TITAS000                       *PULISCE PER STAMPA
011100990622     C                   CLEAR                   TITAS010                       *PULISCE PER STAMPA
011200990622     C                   CLEAR                   TITASP00                       *PULISCE PER STAMPA
011300971002     C*                    MOVELERR,1     $DERR            *BOLLA NON TROVATA
011400971002     C*                    EXSR STAERR
011500970926   X1C                   ELSE
011600970926    2C     *IN97         DOWEQ     *OFF
011700000000     C*
011800000000     C* CONTROLLA VALIDITA' BOLLA
011900000000     C                   EXSR      CHKTAS
012000970926    3C     WRECOK        IFEQ      'S'                                          *BOLLA VALIDA
012100990622     C* SE NUMERO SPEDIZIONI PER STOP > 99999 LO SEGNALO ED AGGIORNERO'
012200990622     C* TITAS MEMORIZZANDO 99999
012300990622    4C     STPSET        IFGT      99999
012400970929     C                   MOVEL     ERR(3)        $DERR
012500970929     C                   EXSR      STAERR
012600971014    4C                   END
012700000000     C***
012800000000     C* R I T I R O
012900000000     C***
013000970926    4C     STPTSR        IFEQ      'R'
013100990622    5C     STPSET        IFGT      99999
013200990622     C                   Z-ADD     99998         TASSTR
013300971014     C                   ELSE
013400970926     C                   Z-ADD     STPSET        TASSTR
013500971014    5C                   END
013600990622     C   01              EXCEPT    UPRTAS                                       *AGGIORNA TITAS00F
013700990622     C   02              EXCEPT    UPRT10                                       *AGGIORNA TITAS10F
013800990622     C   03              EXCEPT    UPRTAP                                       *AGGIORNA TITASP0F
013900000000     C*
014000970926X   4C                   ELSE
014100000000     C***
014200000000     C* C O N S E G N A
014300000000     C***
014400990622    5C     STPSET        IFGT      99999
014500990622     C                   Z-ADD     99998         TASSTP
014600971014     C                   ELSE
014700971014     C                   Z-ADD     STPSET        TASSTP
014800971014    5C                   END
014900990622     C   01              EXCEPT    UPCTAS                                       *AGGIORNA TITAS00F
015000990622     C   02              EXCEPT    UPCT10                                       *AGGIORNA TITAS10F
015100990622     C   03              EXCEPT    UPCTAP                                       *AGGIORNA TITASP0F
015200000000     C*
015300970926    4C                   ENDIF
015400970926    3C                   ENDIF
015500950411     C*
015600950427     C                   SETOFF                                       010203
015700990622     C     KEYTAS        READE     TITAS30C                               97
015800970926    2C                   ENDDO
015900000000     C*
016000000000     C* CANCELLA RECORD STOP RICEVUTO
016100970926     C                   DELETE    FNSTP000
016200970926    1C                   END
016300000000     C*
016400000000     C                   ENDSR
016500000000     C*------------------------------------------------------------------------*
016600990622     C* CHKTAS - CONTROLLA VALIDITA' RECORD DI TITAS
016700000000     C*------------------------------------------------------------------------*
016800000000     C     CHKTAS        BEGSR
016900000000     C*
017000000000     C                   MOVEL     'S'           WRECOK
017100971002     C     TASTBL        IFNE      SAVTBL
017200971002     C                   MOVEL     'TB'          COD
017300971002     C                   MOVEL(P)  TASTBL        KEY
017400971002     C     KTAB          CHAIN     TABEL                              31
017500971002     C  N31              MOVEL     TBLUNI        DSTB
017600971002     C   31              CLEAR                   DSTB
017700971002     C                   MOVE      TASTBL        SAVTBL
017800971002     C                   END
017900000000     C*
018000971002     C* ESCLUDE ALCUNI TIPI BOLLA: 'AP' E RECUPERI
018100950307IF  1C     TASTBL        IFEQ      'AP'
018200971002IF  1C     §TBRBL        OREQ      'R'
018300000000     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
018400950307E   1C                   ENDIF
018500950307     C*
018600000000     C                   ENDSR
018700000000     C*------------------------------------------------------------------------*
018800000000     C* STAERR - STAMPA L'ERRORE IN RICEZIONE
018900000000     C*------------------------------------------------------------------------*
019000000000     C     STAERR        BEGSR
019100000000     C*
019200000000     C* APERTURA FILE PRINTER
019300000000IF  1C     $OPEN         IFEQ      '0'                                          *MAI APERTO
019400000000     C                   OPEN      QSYSPRT
019500000000     C                   MOVEL     '1'           $OPEN                          *GIA' APERTO
019600971028     C                   MOVEL     '1'           *INOF                          *OVERFLOW
019700000000E   1C                   ENDIF
019800000000     C*
019900000000     C* STAMPA TESTATA PER SALTO PAGINA
020000000000IF  1C     *INOF         IFEQ      '1'
020100000000     C                   EXCEPT    TES
020200000000     C                   MOVEL     '0'           *INOF
020300000000E   1C                   ENDIF
020400000000     C* STAMPA ERRORE
020500000000     C                   EXCEPT    DET
020600000000     C*
020700000000     C                   ENDSR
020800000000     C*------------------------------------------------------------------------*
020900000000     C* CAEXR - APERTURA MEMBRO FILE DI RICEZIONE RITIRO
021000000000     C*------------------------------------------------------------------------*
021100000000     C     CAEXR         BEGSR
021200000000     C*
021300970926     C* MEMBRO DA APRIRE
021400000000     C                   MOVE      ')'           VAR11            11
021500000000     C                   MOVEL     MBRFIL        VAR11
021600000000     C                   MOVE      '))'          VAR12            12
021700000000     C                   MOVEL     MBRFIL        VAR12
021800000000     C                   Z-ADD     55            LUNG
021900000000     C*
022000970926     C* ALCOBJ
022100040803     C**                 SETOFF                                       98
022200040803     C**                 MOVE      VAR12         CMD1
022300040803     C**                 MOVEL     *BLANKS       COMMAN
022400040803     C**                 MOVEA     CMD1(1)       COMMAN
022500040803     C**                 CALL      'QCMDEXC'                            98
022600040803     C**                 PARM                    COMMAN           80
022700040803     C**                 PARM                    LUNG             15 5
022800000000     C*
022900000000     C* OVRDBF --> APRE IL MEMBRO VOLUTO
023000040803IF  1C**   *IN98         IFEQ      *OFF
023100000000     C                   SETOFF                                       98
023200000000     C                   MOVE      VAR11         CMD2
023300000000     C                   MOVEL     *BLANKS       COMMAN
023400000000     C                   MOVEA     CMD2(1)       COMMAN
023500000000     C                   CALL      'QCMDEXC'                            98
023600040803     C                   PARM                    COMMAN           80
023700040803     C                   PARM                    LUNG             15 5
023800000000     C* OPEN
023900040803IF  2C**   *IN98         IFEQ      *OFF
024000000000     C                   SETOFF                                       98
024100970926     C                   OPEN      FNSTP00R                             98
024200040803E   2C**                 ENDIF
024300040803E   1C**                 ENDIF
024400000000     C*
024500000000     C                   ENDSR
024600000000     C*------------------------------------------------------------------------*
024700000000     C* *INZSR - IMPOSTAZIONI INIZIALI
024800000000     C*------------------------------------------------------------------------*
024900000000     C     *INZSR        BEGSR
025000000000     C*--------------------
025100000000     C* RICEVIMENTO PARAMETRI
025200000000     C*--------------------
025300000000     C     *ENTRY        PLIST
025400000000     C                   PARM                    KPJBA
025500000000     C*--------------------
025600000000     C* CAMPI RIFERITI AL DATA BASE
025700000000     C*--------------------
025800990622     C     *LIKE         DEFINE    TASAAS        KTAAAS                         *TITAS30C
025900000000     C     *LIKE         DEFINE    TASLNP        KTALNP
026000000000     C     *LIKE         DEFINE    TASNRS        KTANRS
026100000000     C     *LIKE         DEFINE    TASNSP        KTANSP
026200971002     C     *LIKE         DEFINE    TASTBL        SAVTBL
026300971002     C     *LIKE         DEFINE    TBLCOD        COD
026400971002     C     *LIKE         DEFINE    TBLKEY        KEY
026500000000     C*--------------------
026600000000     C* CHIAVI DI LETTURA
026700000000     C*--------------------
026800990622     C* CHIAVE LETTURA TITAS30C -
026900000000     C     KEYTAS        KLIST
027000000000     C                   KFLD                    KTAAAS                         *ANNO
027100000000     C                   KFLD                    KTALNP                         *LINEA DI PARTENZA
027200000000     C                   KFLD                    KTANRS                         *NUMERO DI SERIE
027300000000     C                   KFLD                    KTANSP                         *NUMERO DI SPEDIZION
027400971002     C*
027500971002     C     KTAB          KLIST
027600971002     C                   KFLD                    CODUT
027700971002     C                   KFLD                    COD
027800971002     C                   KFLD                    KEY
027900971002     C                   Z-ADD     1             CODUT             1 0
028000000000     C*--------------------
028100000000     C* CAMPI DI CONTROLLO
028200000000     C*--------------------
028300000000     C                   MOVEL     'S'           WRECOK            1            *RECORD VALIDO
028400971002     C                   MOVEL     *BLANKS       $DERR            60            *DESCRIZIONE ERRORE
028500000000     C                   MOVEL     '0'           $OPEN             1            *APERTURA FILE STAMP
028600000000     C                   TIME                    ORA               6 0          *ORA
028700970926     C                   TIME                    WTIME            14 0          ORA & DATA
028800970926     C                   MOVE      WTIME         DATA              8 0          GG/MM/AAAA
028900000000     C*
029000000000     C* REPERISCE DATI UTENTE
029100020716
029200020716     c     *dtaara       define    §azute        azuteds
029300020716     c     *dtaara       define    §datiute      ddatiute
029400020716
029500020716     c                   in(E)     *dtaara
029600020716     c                   if        %Error or rsut = *blanks
029700020716     c                   clear                   tibs34ds
029800020716     c                   call      'TIBS34R'
029900020716     c                   parm                    Tibs34Ds
030000020716     c                   in        *dtaara
030100020716     c                   endif
030200000000     C*
030300000000     C                   ENDSR
030400000000     O*------------------------------------------------------------------------*
030500000000     O* STAMPA ERRORI
030600000000     O*------------------------------------------------------------------------*
030700000000     OQSYSPRT   E            TES              02
030800000000     O                       STA(1)              66
030900000000     O                       STA(2)             132
031000020716     O**!!!                  DSFIRS              20
031100020716     o                       Rsut                20
031200970926     O                       DATA               108 '  /  /    '
031300000000     O                       ORA                119 '  :  :  '
031400000000     O                       PAGE          Z    132
031500000000     O*------------------------
031600000000     O          E            TES              04
031700000000     O                       STA(3)              66
031800000000     O                       STA(3)             132
031900000000     O*------------------------
032000000000     O          E            TES              05
032100000000     O                       STA(4)              66
032200000000     O*------------------------
032300000000     O          E            TES              06
032400000000     O                       STA(3)              66
032500971002     O                       STA(3)             132
032600000000     O*------------------------------------------------------------------------*
032700000000     O          E            DET         1
032800970926     O                       STPAAS        Z      4
032900970926     O                       STPLNP        Z      8
033000970926     O                       STPNRS        Z     11
033100970926     O                       STPNSP        Z     19
033200000000     O                       TASTBL              22
033300970926     O                       STPTSR              25
033400970926     O                       MBR4                33
033500971002     O                       $DERR               97
033600000000     O*------------------------------------------------------------------------*
033700990622     O* AGGIORNAMENTO RECORD DI TITAS30C - R I T I R O
033800000000     O*------------------------------------------------------------------------*
033900990622     OTITAS000  E            UPRTAS
034000000000     O                       TASSTR
034100990622     OTITAS010  E            UPRT10
034200000000     O                       TASSTR
034300990622     OTITASP00  E            UPRTAP
034400960412     O                       TASSTR
034500000000     O*------------------------------------------------------------------------*
034600990622     O* AGGIORNAMENTO RECORD DI TITAS30C - C O N S E G N A
034700000000     O*------------------------------------------------------------------------*
034800990622     OTITAS000  E            UPCTAS
034900000000     O                       TASSTP
035000990622     OTITAS010  E            UPCT10
035100000000     O                       TASSTP
035200990622     OTITASP00  E            UPCTAP
035300960412     O                       TASSTP
035400000000     O*------------------------------------------------------------------------*
035500000000**         CMD1
035600970926ALCOBJ OBJ((FNSTP00R *FILE *EXCL           M234567890))
035700970926**         CMD2
035800970926OVRDBF FILE(FNSTP00R)                   MBR(M234567890)
035900000000**         CMD5
036000970926DLCOBJ OBJ((FNSTP00R *FILE *EXCL           M234567890))
036100000000**         ERR
036200990622RECORD RICEVUTO ESCLUSO: BOLLA NON TROVATA IN TITAS
036300971002
036400990622RECORD RICEVUTO ESLCUSO: NUMERO SPEDIZ. PER STOP > DI 99999
0365000000004
0366000000005
036700000000**         STA
036800000000XXXXXXXXXXXXXXXXXXXX                   ** ERRORI IN RICEZIONE STOP    1
036900971014 - FNSTP00R **      TRTR45R     99/99/9999   99:99:99    PAG. 9999    2
037000000000------------------------------------------------------------------    3
037100970926S P E D I Z I O N E    C/R  MEMBRO   DESCRIZIONE ERRORE               4
0372009709269999 999 99 9999999 XX  X    XXXX    12345678901234567890123456789    5
0373009710020123456789012345678901234567890                                       6
0374009710027
037500000000 8
037600000000 9
037700000000 10
