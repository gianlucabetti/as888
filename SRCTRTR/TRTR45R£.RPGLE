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
001100000000     FCMRIC01L  UF   E           K DISK
001200000000     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
001300000000     F                                     USROPN
001400000000     D*------------------------------------------------------------------------*
001500000000     D* SCHIERE
001600000000     D*------------------------------------------------------------------------*
001700000000     D*------------------
001800000000     D* COMANDI QCMDEXC
001900000000     D*------------------
002000970926     D CMD1            S             55    DIM(1) CTDATA PERRCD(1)               ALCOBJ
002100970926     D CMD2            S             55    DIM(1) CTDATA PERRCD(1)               OVRDBF
002200000000     D CMD5            S             55    DIM(1) CTDATA PERRCD(1)              *DLCOBJ
002300000000     D*-------------------
002400000000     D* ERRORI DI ELABORAZIONE
002500000000     D*-------------------
002600971002     D ERR             S             60    DIM(5) CTDATA PERRCD(1)
002700000000     D*-------------------
002800000000     D* RIGHE DI STAMPA
002900000000     D*-------------------
003000000000     D STA             S             66    DIM(10) CTDATA PERRCD(1)
003100000000     D*------------------
003200000000     D* ARCHITETTURA
003300000000     D*------------------
003400000000     D KPJBA         E DS
003500000000     D  LIBSYS                92    100
003600000000     D  LEGFIL               483    492
003700000000     D  MBRFIL               493    502
003800950315     D  MBR4                 493    496
003900000000     D*----------------
004000000000     D* DS REPERIMENTO DATI UTENTE
004100000000     D*----------------
004200020716     D**!!! DSFIL         E DS
004300971002     D DSTB          E DS
004400020716
004500020716     d DdatiUte      e ds
004600020716     d AzuteDs       e ds                  ExtName(Azute00f)
004700020716     d Tibs34Ds      e ds                  Inz
004800020716
004900000000     I*------------------------------------------------------------------------*
005000000000     I* INPUT
005100000000     I*------------------------------------------------------------------------*
005200000000     I*------------------
005300000000     I* INDICATORI DI LETTURA
005400000000     I*------------------
005500990622     ITITAS000      01
005600990622     ITITAS010      02
005700990622     ITITASP00      03
005800000000     C*------------------------------------------------------------------------*
005900000000     C* MAIN LINE
006000000000     C*------------------------------------------------------------------------*
006100970926     C* APERTURA MEMBRO DI RICEZIONE
006200970926     C                   EXSR      CAEXR
006300971002     C                   CLEAR                   SAVTBL
006400970926     C*
006500970926     C* APERTURA RIUSCITA
006600970926IF  1C     *IN98         IFEQ      *OFF
006700970926     C                   READ      FNSTP00R                               30
006800970926     C     *IN30         DOWEQ     *OFF
006900970926     C                   EXSR      AGGTAS
007000970926     C                   READ      FNSTP00R                               30
007100970926     C                   ENDDO
007200970926     C* AGGIORNA ARCHIVIO DI LOG
007300970926     C     LEGFIL        CHAIN     CMRIC01L                           99
007400970926     C     *IN99         IFEQ      *OFF
007500970926     C                   Z-ADD     NREC          RICNR1
007600970926     C                   MOVE      '1'           RICFL2
007700970926     C                   UPDATE    CMRIC000
007800970926     C                   ENDIF
007900970926     C* CHIUSURA MEMBRO DI RICEZIONE
008000970926     C                   CLOSE     FNSTP00R
008100970926     C*
008200970926     C                   MOVE      ')'           VAR11
008300970926     C                   MOVEL     MBRFIL        VAR11
008400970926     C                   MOVE      '))'          VAR12
008500970926     C                   MOVEL     MBRFIL        VAR12
008600970926     C* DISALLOCAZIONE
008700970926     C                   MOVE      VAR12         CMD5
008800970926     C                   MOVEL     *BLANKS       COMMAN
008900970926     C                   MOVEA     CMD5(1)       COMMAN
009000970926     C                   CALL      'QCMDEXC'
009100970926     C                   PARM                    COMMAN
009200970926     C                   PARM                    LUNG
009300970926     C*
009400970926     C* CHIUSURA FILE PRINTER
009500970926IF  2C     $OPEN         IFEQ      '1'                                          *MAI APERTO PRTFILE
009600970926     C                   CLOSE     QSYSPRT
009700970926E   2C                   ENDIF
009800970926     C*
009900970926E   1C                   ENDIF
010000000000     C*
010100000000     C                   SETON                                        LR
010200000000     C*------------------------------------------------------------------------*
010300970926     C* AGGTAS - AGGIORNA LE BOLLE E CANCELLA IL RECORD RICEVUTO
010400000000     C*------------------------------------------------------------------------*
010500970926     C     AGGTAS        BEGSR
010600000000     C*
010700950427     C                   SETOFF                                       010203      *LETTURA FORMATI
010800000000     C*
010900000000     C* CONTROLLA ESISTENZA BOLLA
011000970926     C                   Z-ADD     STPAAS        KTAAAS
011100970926     C                   Z-ADD     STPLNP        KTALNP
011200970926     C                   Z-ADD     STPNRS        KTANRS
011300970926     C                   Z-ADD     STPNSP        KTANSP
011400990622     C     KEYTAS        SETLL     TITAS30C                           97
011500990622     C     KEYTAS        READE     TITAS30C                               97
011600970926     C* INESISTENTE
011700970926IF  1C     *IN97         IFEQ      *ON
011800990622     C                   CLEAR                   TITAS000                       *PULISCE PER STAMPA
011900990622     C                   CLEAR                   TITAS010                       *PULISCE PER STAMPA
012000990622     C                   CLEAR                   TITASP00                       *PULISCE PER STAMPA
012100971002     C*                    MOVELERR,1     $DERR            *BOLLA NON TROVATA
012200971002     C*                    EXSR STAERR
012300970926   X1C                   ELSE
012400970926    2C     *IN97         DOWEQ     *OFF
012500000000     C*
012600000000     C* CONTROLLA VALIDITA' BOLLA
012700000000     C                   EXSR      CHKTAS
012800970926    3C     WRECOK        IFEQ      'S'                                          *BOLLA VALIDA
012900990622     C* SE NUMERO SPEDIZIONI PER STOP > 99999 LO SEGNALO ED AGGIORNERO'
013000990622     C* TITAS MEMORIZZANDO 99999
013100990622    4C     STPSET        IFGT      99999
013200970929     C                   MOVEL     ERR(3)        $DERR
013300970929     C                   EXSR      STAERR
013400971014    4C                   END
013500000000     C***
013600000000     C* R I T I R O
013700000000     C***
013800970926    4C     STPTSR        IFEQ      'R'
013900990622    5C     STPSET        IFGT      99999
014000990622     C                   Z-ADD     99998         TASSTR
014100971014     C                   ELSE
014200970926     C                   Z-ADD     STPSET        TASSTR
014300971014    5C                   END
014400990622     C   01              EXCEPT    UPRTAS                                       *AGGIORNA TITAS00F
014500990622     C   02              EXCEPT    UPRT10                                       *AGGIORNA TITAS10F
014600990622     C   03              EXCEPT    UPRTAP                                       *AGGIORNA TITASP0F
014700000000     C*
014800970926X   4C                   ELSE
014900000000     C***
015000000000     C* C O N S E G N A
015100000000     C***
015200990622    5C     STPSET        IFGT      99999
015300990622     C                   Z-ADD     99998         TASSTP
015400971014     C                   ELSE
015500971014     C                   Z-ADD     STPSET        TASSTP
015600971014    5C                   END
015700990622     C   01              EXCEPT    UPCTAS                                       *AGGIORNA TITAS00F
015800990622     C   02              EXCEPT    UPCT10                                       *AGGIORNA TITAS10F
015900990622     C   03              EXCEPT    UPCTAP                                       *AGGIORNA TITASP0F
016000000000     C*
016100970926    4C                   ENDIF
016200970926    3C                   ENDIF
016300950411     C*
016400950427     C                   SETOFF                                       010203
016500990622     C     KEYTAS        READE     TITAS30C                               97
016600970926    2C                   ENDDO
016700000000     C*
016800000000     C* CANCELLA RECORD STOP RICEVUTO
016900970926     C                   ADD       1             NREC
017000970926     C                   DELETE    FNSTP000
017100970926    1C                   END
017200000000     C*
017300000000     C                   ENDSR
017400000000     C*------------------------------------------------------------------------*
017500990622     C* CHKTAS - CONTROLLA VALIDITA' RECORD DI TITAS
017600000000     C*------------------------------------------------------------------------*
017700000000     C     CHKTAS        BEGSR
017800000000     C*
017900000000     C                   MOVEL     'S'           WRECOK
018000971002     C     TASTBL        IFNE      SAVTBL
018100971002     C                   MOVEL     'TB'          COD
018200971002     C                   MOVEL(P)  TASTBL        KEY
018300971002     C     KTAB          CHAIN     TABEL                              31
018400971002     C  N31              MOVEL     TBLUNI        DSTB
018500971002     C   31              CLEAR                   DSTB
018600971002     C                   MOVE      TASTBL        SAVTBL
018700971002     C                   END
018800000000     C*
018900971002     C* ESCLUDE ALCUNI TIPI BOLLA: 'AP' E RECUPERI
019000950307IF  1C     TASTBL        IFEQ      'AP'
019100971002IF  1C     §TBRBL        OREQ      'R'
019200000000     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
019300950307E   1C                   ENDIF
019400950307     C*
019500000000     C                   ENDSR
019600000000     C*------------------------------------------------------------------------*
019700000000     C* STAERR - STAMPA L'ERRORE IN RICEZIONE
019800000000     C*------------------------------------------------------------------------*
019900000000     C     STAERR        BEGSR
020000000000     C*
020100000000     C* APERTURA FILE PRINTER
020200000000IF  1C     $OPEN         IFEQ      '0'                                          *MAI APERTO
020300000000     C                   OPEN      QSYSPRT
020400000000     C                   MOVEL     '1'           $OPEN                          *GIA' APERTO
020500971028     C                   MOVEL     '1'           *INOF                          *OVERFLOW
020600000000E   1C                   ENDIF
020700000000     C*
020800000000     C* STAMPA TESTATA PER SALTO PAGINA
020900000000IF  1C     *INOF         IFEQ      '1'
021000000000     C                   EXCEPT    TES
021100000000     C                   MOVEL     '0'           *INOF
021200000000E   1C                   ENDIF
021300000000     C* STAMPA ERRORE
021400000000     C                   EXCEPT    DET
021500000000     C*
021600000000     C                   ENDSR
021700000000     C*------------------------------------------------------------------------*
021800000000     C* CAEXR - APERTURA MEMBRO FILE DI RICEZIONE RITIRO
021900000000     C*------------------------------------------------------------------------*
022000000000     C     CAEXR         BEGSR
022100000000     C*
022200970926     C* MEMBRO DA APRIRE
022300000000     C                   MOVE      ')'           VAR11            11
022400000000     C                   MOVEL     MBRFIL        VAR11
022500000000     C                   MOVE      '))'          VAR12            12
022600000000     C                   MOVEL     MBRFIL        VAR12
022700000000     C                   Z-ADD     55            LUNG
022800000000     C*
022900970926     C* ALCOBJ
023000000000     C                   SETOFF                                       98
023100000000     C                   MOVE      VAR12         CMD1
023200000000     C                   MOVEL     *BLANKS       COMMAN
023300000000     C                   MOVEA     CMD1(1)       COMMAN
023400000000     C                   CALL      'QCMDEXC'                            98
023500000000     C                   PARM                    COMMAN           80
023600000000     C                   PARM                    LUNG             15 5
023700000000     C*
023800000000     C* OVRDBF --> APRE IL MEMBRO VOLUTO
023900000000IF  1C     *IN98         IFEQ      *OFF
024000000000     C                   SETOFF                                       98
024100000000     C                   MOVE      VAR11         CMD2
024200000000     C                   MOVEL     *BLANKS       COMMAN
024300000000     C                   MOVEA     CMD2(1)       COMMAN
024400000000     C                   CALL      'QCMDEXC'                            98
024500000000     C                   PARM                    COMMAN
024600000000     C                   PARM                    LUNG
024700000000     C* OPEN
024800000000IF  2C     *IN98         IFEQ      *OFF
024900000000     C                   SETOFF                                       98
025000970926     C                   OPEN      FNSTP00R                             98
025100000000E   2C                   ENDIF
025200000000E   1C                   ENDIF
025300000000     C*
025400000000     C                   ENDSR
025500000000     C*------------------------------------------------------------------------*
025600000000     C* *INZSR - IMPOSTAZIONI INIZIALI
025700000000     C*------------------------------------------------------------------------*
025800000000     C     *INZSR        BEGSR
025900000000     C*--------------------
026000000000     C* RICEVIMENTO PARAMETRI
026100000000     C*--------------------
026200000000     C     *ENTRY        PLIST
026300000000     C                   PARM                    KPJBA
026400000000     C*--------------------
026500000000     C* CAMPI RIFERITI AL DATA BASE
026600000000     C*--------------------
026700990622     C     *LIKE         DEFINE    TASAAS        KTAAAS                         *TITAS30C
026800000000     C     *LIKE         DEFINE    TASLNP        KTALNP
026900000000     C     *LIKE         DEFINE    TASNRS        KTANRS
027000000000     C     *LIKE         DEFINE    TASNSP        KTANSP
027100971002     C     *LIKE         DEFINE    TASTBL        SAVTBL
027200971002     C     *LIKE         DEFINE    TBLCOD        COD
027300971002     C     *LIKE         DEFINE    TBLKEY        KEY
027400000000     C*--------------------
027500000000     C* CHIAVI DI LETTURA
027600000000     C*--------------------
027700990622     C* CHIAVE LETTURA TITAS30C -
027800000000     C     KEYTAS        KLIST
027900000000     C                   KFLD                    KTAAAS                         *ANNO
028000000000     C                   KFLD                    KTALNP                         *LINEA DI PARTENZA
028100000000     C                   KFLD                    KTANRS                         *NUMERO DI SERIE
028200000000     C                   KFLD                    KTANSP                         *NUMERO DI SPEDIZION
028300971002     C*
028400971002     C     KTAB          KLIST
028500971002     C                   KFLD                    CODUT
028600971002     C                   KFLD                    COD
028700971002     C                   KFLD                    KEY
028800971002     C                   Z-ADD     1             CODUT             1 0
028900000000     C*--------------------
029000000000     C* CAMPI DI CONTROLLO
029100000000     C*--------------------
029200000000     C                   MOVEL     'S'           WRECOK            1            *RECORD VALIDO
029300971002     C                   MOVEL     *BLANKS       $DERR            60            *DESCRIZIONE ERRORE
029400000000     C                   MOVEL     '0'           $OPEN             1            *APERTURA FILE STAMP
029500000000     C                   TIME                    ORA               6 0          *ORA
029600970926     C                   TIME                    WTIME            14 0          ORA & DATA
029700970926     C                   MOVE      WTIME         DATA              8 0          GG/MM/AAAA
029800970926     C                   Z-ADD     *ZEROS        NREC              7 0          *N° RECORD CANCELLAT
029900000000     C*
030000000000     C* REPERISCE DATI UTENTE
030100020716     C**!!!              CALL      'XFIL'
030200020716     C**!!!              PARM                    DSFIL
030300020716
030400020716     c     *dtaara       define    §azute        azuteds
030500020716     c     *dtaara       define    §datiute      ddatiute
030600020716
030700020716     c                   in(E)     *dtaara
030800020716     c                   if        %Error or rsut = *blanks
030900020716     c                   clear                   tibs34ds
031000020716     c                   call      'TIBS34R'
031100020716     c                   parm                    Tibs34Ds
031200020716     c                   in        *dtaara
031300020716     c                   endif
031400000000     C*
031500000000     C                   ENDSR
031600000000     O*------------------------------------------------------------------------*
031700000000     O* STAMPA ERRORI
031800000000     O*------------------------------------------------------------------------*
031900000000     OQSYSPRT   E            TES              02
032000000000     O                       STA(1)              66
032100000000     O                       STA(2)             132
032200020716     O**!!!                  DSFIRS              20
032300020716     o                       Rsut                20
032400970926     O                       DATA               108 '  /  /    '
032500000000     O                       ORA                119 '  :  :  '
032600000000     O                       PAGE          Z    132
032700000000     O*------------------------
032800000000     O          E            TES              04
032900000000     O                       STA(3)              66
033000000000     O                       STA(3)             132
033100000000     O*------------------------
033200000000     O          E            TES              05
033300000000     O                       STA(4)              66
033400000000     O*------------------------
033500000000     O          E            TES              06
033600000000     O                       STA(3)              66
033700971002     O                       STA(3)             132
033800000000     O*------------------------------------------------------------------------*
033900000000     O          E            DET         1
034000970926     O                       STPAAS        Z      4
034100970926     O                       STPLNP        Z      8
034200970926     O                       STPNRS        Z     11
034300970926     O                       STPNSP        Z     19
034400000000     O                       TASTBL              22
034500970926     O                       STPTSR              25
034600970926     O                       MBR4                33
034700971002     O                       $DERR               97
034800000000     O*------------------------------------------------------------------------*
034900990622     O* AGGIORNAMENTO RECORD DI TITAS30C - R I T I R O
035000000000     O*------------------------------------------------------------------------*
035100990622     OTITAS000  E            UPRTAS
035200000000     O                       TASSTR
035300990622     OTITAS010  E            UPRT10
035400000000     O                       TASSTR
035500990622     OTITASP00  E            UPRTAP
035600960412     O                       TASSTR
035700000000     O*------------------------------------------------------------------------*
035800990622     O* AGGIORNAMENTO RECORD DI TITAS30C - C O N S E G N A
035900000000     O*------------------------------------------------------------------------*
036000990622     OTITAS000  E            UPCTAS
036100000000     O                       TASSTP
036200990622     OTITAS010  E            UPCT10
036300000000     O                       TASSTP
036400990622     OTITASP00  E            UPCTAP
036500960412     O                       TASSTP
036600000000     O*------------------------------------------------------------------------*
036700000000**         CMD1
036800970926ALCOBJ OBJ((FNSTP00R *FILE *EXCL           M234567890))
036900970926**         CMD2
037000970926OVRDBF FILE(FNSTP00R)                   MBR(M234567890)
037100000000**         CMD5
037200970926DLCOBJ OBJ((FNSTP00R *FILE *EXCL           M234567890))
037300000000**         ERR
037400990622RECORD RICEVUTO ESCLUSO: BOLLA NON TROVATA IN TITAS
037500971002
037600990622RECORD RICEVUTO ESLCUSO: NUMERO SPEDIZ. PER STOP > DI 99999
0377000000004
0378000000005
037900000000**         STA
038000000000XXXXXXXXXXXXXXXXXXXX                   ** ERRORI IN RICEZIONE STOP    1
038100971014 - FNSTP00R **      TRTR45R     99/99/9999   99:99:99    PAG. 9999    2
038200000000------------------------------------------------------------------    3
038300970926S P E D I Z I O N E    C/R  MEMBRO   DESCRIZIONE ERRORE               4
0384009709269999 999 99 9999999 XX  X    XXXX    12345678901234567890123456789    5
0385009710020123456789012345678901234567890                                       6
0386009710027
038700000000 8
038800000000 9
038900000000 10
