000100000000     H*------------------------------------------------------------------------*
000200000000     H* REPERISCE RICAVI/COMPETENZE/VOCI DI C/ECONOMICO PER BOLLA - VISUALIZZA *
000300000000     H*------------------------------------------------------------------------*
000400000000     H DECEDIT('0,') DATEDIT(*DMY.)
000500000000     F*------------------------------------------------------------------------*
000600000000     F* DATA BASE
000700000000     F*------------------------------------------------------------------------*
000800000000     FTREC79D   CF   E             WORKSTN INFDS(DEVDS1)
000900000000     F                                     SFILE(EC79DVS:SFLULT)
001000000000     F                                     SFILE(EC79DVM:MSGULT)
001100020620     F***TITAS30C  IF   E           K DISK
001200020620     F****---                                 IGNORE(TITAS000)
001300020620     F***                                     IGNORE(TITASP00)
001400000000     FECECT01L  IF   E           K DISK
001500000000     FECEVT01L  IF   E           K DISK
001600000000     FTABEL00F  IF   E           K DISK
001700000000     D*------------------------------------------------------------------------*
001800000000     D* SCHIERE
001900000000     D*------------------------------------------------------------------------*
002000000000     D*------------------
002100000000     D* SCHIERE PER MESSAGGI, OPZIONI E FUNZIONI
002200000000     D*------------------
002300000000     D MSG             S             78    DIM(20) CTDATA PERRCD(1)
002400000000     D OPZ             S             40    DIM(24) CTDATA PERRCD(1)
002500000000     D FUN2            S             40    DIM(31) CTDATA PERRCD(1)
002600000000     D FUN             S             40    DIM(31)
002700000000     D*------------------
002800000000     D* SINGOLE SCHIERE ALL'INTERNO DELLA RIGA FUNZIONI
002900000000     D*------------------
003000000000     D FFU             S              6    DIM(31)
003100000000     D FDE             S             19    DIM(31)
003200000000     D FAT             S              1    DIM(31)
003300000000     D*------------------
003400000000     D* SINGOLE SCHIERE ALL'INTERNO DELLA RIGA OPZIONI
003500000000     D*------------------
003600000000     D OPD             S              2    DIM(24)
003700000000     D OPS             S              2    DIM(24)
003800000000     D ODE             S             19    DIM(24)
003900000000     D OAT             S              1    DIM(24)
004000000000     D*------------------
004100000000     D* TABELLA DESCRIZIONI A VIDEO
004200000000     D*------------------
004300000000     D FUV             S             19    DIM(8)
004400000000     D OPV             S             19    DIM(8)
004500000000     D*------------------
004600000000     D* TABELLA TASTI FUNZIONALI PREMUTI
004700000000     D*------------------
004800000000     D KEYS            S              1    DIM(40)
004900000000     D*------------------
005000000000     D* IMPORTI RICAVI E COMPETENZE
005100000000     D*------------------
005200020102     D IM              S             13  4 DIM(200)
005300000000     D CIM             S              3  0 DIM(200)
005400000000     D TIM             S              1    DIM(200)
005500000000     D PIM             S              3  0 DIM(200)
005600000000     D*-------------------
005700000000     D* SCHIERE DI RITORNO IMPORTI E RICAVI -DOPO- NUMERICHE
005800000000     D*-------------------
005900000000     D C$D             S              3  0 DIM(50)
006000020102     D I$D             S             13  4 DIM(50)
006100000000     D P$D             S              3  0 DIM(50)
006200000000     D*-------------------
006300000000     D* SCHIERE DI RITORNO IMPORTI E RICAVI -DOPO- ALFANUMERICHE
006400000000     D*-------------------
006500950125     D CAD             S              2    DIM(50)
006600020102     D IAD             S              7    DIM(50)
006700950125     D PAD             S              2    DIM(50)
006800950405     D*****************************************************************
006900950405     D* SCHIERE CODICI E IMPORTI IN INPUT E OUTPUT AL PGM
007000950405     D CAN             S              2    DIM(50)
007100020102     D IAN             S              7    DIM(50)
007200950405     D*****************************************************************
007300950405     D* DS PER PASSAGGIO CAMPI PACKED -CODICE-
007400950405     D                 DS
007500950405     D  DSWCN                  1      3P 0
007600950405     D  DSWCA                  1      3
007700020102     D  DSWIN                  4     13P 4
007800020102     D  DSWIA                  4     13
007900000000     D*------------------------------------------------------------------------*
008000000000     D* INPUT
008100000000     D*------------------------------------------------------------------------*
008101031212     D*------------------
008102031212     D* DS GESTIONE RICERCA CONTROLLO TABELLE
008103031212     D*------------------
008104031212     D TIBS02DS      E DS
008105031212     D*------------------
008106031212     D* DS LETTURA TABELLA CO1 tipo data x calcolo
008107031212     D*------------------
008108031212     D DCO1          E DS
008200000000     D*---------------------
008300000000     D* RIDEFINIZIONE SCHIERA -OPD- (OPZIONI)
008400000000     D*---------------------
008500000000     D DSOPZ         E DS
008600000000     D  OPZRIG                 1     40
008700000000     D*---------------------
008800000000     D* RIDEFINIZIONE SCHIERA -FUD- (FUNZIONI)
008900000000     D*---------------------
009000000000     D DSFUN         E DS
009100000000     D  FUNRIG                 1     40
009200000000     D*---------------------
009300000000     D* PARAMETRI CHIAMATA CL XMSG (GESTIONE MESSAGGI AL PGM)
009400000000     D*---------------------
009500000000     D DSMSG         E DS
009600000000     D*---------------------
009700020923     D* DS REPERIMENTO DATI UTENTE
009800020923     D*-------------------
009900020923     D TIBS34DS      E DS
010000020923     D DDATIUTE      E DS
010100020923     D AZUTEDS       E DS                  extname(AZUTE00F)
010200001204     D*-------------
010300001204     D* DS DATI SIMULAZIONE
010400001204     D*-------------
010500001204     D DSD$G         E DS
010600000000     D*---------------------
010700000000     D* DS PER GESTIONE TASTI FUNZIONALI
010800000000     D*---------------------
010900000000     D DSKEYS        E DS
011000000000     D  KEYRIG                 1     40
011100000000     D*---------------------
011200000000     D* RIDEFINIZIONE INFDS STATO DEL PROGRAMMA
011300000000     D*---------------------
011400000000     D                SDS
011500000000     D  USER                 254    263
011600000000     D*---------------------
011700000000     D* RIDEFINIZIONE INFDS DEL FILE VIDEO
011800000000     D*---------------------
011900000000     D DEVDS1          DS
012000000000     D* CHIAVE FUNZIONALE BATTUTA
012100000000     D  KEY                  369    369
012200000000     D* POSIZIONE CURSORE (IN HEX)
012300000000     D  CURSOR               370    371
012400000000     D* RRN DELLA PRIMA RIGA PAG. CORRENTE SFL VISUALIZZATA
012500000000     D  FRTSFL               378    379B 0
012600000000     D*---------------------
012700000000     D* ARCHITETTURA
012800000000     D*---------------------
012900031212     D KPJBA         E DS
013000000000     D*-------------------
013100000000     D* DS ESTERNA PER CALCOLO IMPORTI E RICAVI
013200000000     D*-------------------
013300020620     D DS$ECO        E DS
013400000000     D*-------------------
013500000000     D* DS ESTERNA PER CALCOLO IMPORTI E RICAVI - RIDEFINIZIONE IMMAGINE BOLLA -
013600000000     D*-------------------
013700000000     D TRECDS        E DS
013800000000     D  D$PDS1                 1    250
013900000000     D  D$PDS2               251    500
014000000000     D  D$DDS1               501    750
014100000000     D  D$DDS2               751   1000
014200000000     D  D$ODS1              1001   1250
014300000000     D  D$ODS2              1251   1500
014400000000     D*------------------
014500000000     D* PARAMETRI RICEVUTI
014600000000     D*------------------
014700020620     D*PARAM           DS
014800020620     D* PARAAS                 1      4  0
014900020620     D* PARLNP                 5      7  0
015000020620     D* PARNRS                 8      9  0
015100020620     D* PARNSP                10     16  0
015200020620     D* PARTBL                17     18
015300020620     D* PARASI                19     22  0
015400000000     D*-------------------
015500980130     D* DS DATI VARI PER SALDI E C/E -NON UNICA-
015600000000     D*-------------------
015700980130     D DS2K          E DS
015800000000     D*-------------------
015900950124     D* DS PER PASSAGGIO CAMPI PACKED
016000000000     D*-------------------
016100000000     D DSPACK          DS
016200950125     D  DSNUC1                 1      3P 0
016300950125     D  DSNUC2                 1      3
016400020102     D  DSNUI1                 4     13P 4
016500020102     D  DSNUI2                 4     13
016600950125     D* OK
016700000000     D*-------------------
016800000000     D* DS UNIONE DATA SPEDIZIONE
016900000000     D*-------------------
017000000000     D                 DS
017100020620     D  depAAS                 1      4  0
017200020620     D  depMGS                 5      8  0
017300020620     D  D$DDSP                 1      8  0
017400020530     I*---
017500020530     I* DEFINIZIONE DEI TIPI RECORD DEL FILE TITAS30C
017600020530     I*---
017700020620     I***TITAS000      01
017800020620     I***TITAS010      02
017900000000     C*------------------------------------------------------------------------*
018000000000     C* MAIN LINE
018100000000     C*------------------------------------------------------------------------*
018200000000     C*
018300000000     C* CICLO FINO A CHE:
018400000000DO  1C     $FINE         DOUEQ     '1'
018500000000     C                   MOVEL     '0'           $ERR
018600000000     C*
018700000000     C* CONTROLLA GESTIONE VIDEO
018800000000IF  2C     $GES          IFEQ      'VS'
018900000000     C                   EXSR      GESVS
019000000000E   2C                   ENDIF
019100000000     C*
019200000000E   1C                   ENDDO
019300000000     C*
019400000000     C                   SETON                                        LR
019500000000     C*------------------------------------------------------------------------*
019600000000     C* CARVS  - CARICA GESTIONE VIDEO 'SUBFILE'
019700000000     C*------------------------------------------------------------------------*
019800000000     C     CARVS         BEGSR
019900000000     C*
020000000000     C                   MOVEL     'VS'          $GES
020100000000     C*
020200000000     C                   EXSR      CAROPZ
020300000000     C                   EXSR      OUTOPZ
020400000000     C                   MOVEA     FUN2          FUN
020500000000     C                   EXSR      CARFUN
020600000000     C                   EXSR      OUTFUN
020700000000     C*
020800000000     C                   WRITE     EC79DVT
020900000000     C                   WRITE     EC79DVO
021000000000     C                   WRITE     EC79DVF
021100000000     C*
021200000000     C                   EXSR      POSSFL
021300000000     C*
021400000000     C                   ENDSR
021500000000     C*------------------------------------------------------------------------*
021600000000     C* GESVS  - GESTIONE SUBFILE
021700000000     C*------------------------------------------------------------------------*
021800000000     C     GESVS         BEGSR
021900000000     C*
022000000000IF  1C     $TESVD        IFEQ      '1'
022100000000     C                   WRITE     EC79DVT
022200000000     C                   WRITE     EC79DVO
022300000000     C                   WRITE     EC79DVF
022400000000E   1C                   ENDIF
022500000000     C*
022600000000     C                   EXSR      OUTSFL
022700000000     C*
022800000000     C                   EXSR      CONFUN
022900000000     C*
023000000000IF  1C     $ERR          IFEQ      '0'
023100000000SL  2C                   SELECT
023200000000     C                   OTHER
023300000000     C                   EXSR      FUNSFL
023400000000E   2C                   ENDSL
023500000000E   1C                   ENDIF
023600000000     C*
023700000000     C                   ENDSR
023800000000     C*------------------------------------------------------------------------*
023900000000     C*  GESOPZ - GESTIONE OPZIONI
024000000000     C*------------------------------------------------------------------------*
024100000000     C     GESOPZ        BEGSR
024200000000     C*
024300000000SL  1C                   SELECT
024400000000     C                   OTHER
024500000000     C                   MOVEL     MSG(4)        $MSG
024600000000     C                   EXSR      CARMSG
024700000000E   1C                   ENDSL
024800000000     C*
024900000000     C                   ENDSR
025000000000     C*------------------------------------------------------------------------*
025100000000     C* NEWPOS - CONTROLLO DELLA RICHIESTA DI NUOVO POSIZIONAMENTO
025200000000     C*------------------------------------------------------------------------*
025300000000     C     NEWPOS        BEGSR
025400000000     C*
025500000000     C* SWPOSI 0 = NESSUNA RICHIESTA DI POSIZIONAMENTO
025600000000     C*        1 = RICHIESTO POSIZIONAMENTO
025700000000     C                   Z-ADD     0             SWPOSI            7 0
025800000000     C*
025900000000     C* SWMODI 0 = NESSUN AGGIORNAMENTO AFFETTUATO DALL'UTENTE CON L'OPZIONE 2,
026000000000     C*            NON OCCORRE RICARICARE IL SUBFILE
026100000000     C*        1 = AGGIORNAMENTI EFFETTUATI, OCCORRE RICARICARE
026200000000     C                   Z-ADD     0             SWMODI            1 0
026300000000     C*
026400000000     C* CONTROLLO SE RICHIESTO NUOVO POSIZIONAMENTO
026500000000IF  1C     VCCCOM        IFNE      DEPCOM
026600000000     C*
026700000000     C* E' RICHIESTO POSIZIONAMENTO, CONTROLLO I PARAMETRI DI PARTENZA
026800000000     C                   EXSR      CHKPAR
026900000000     C*
027000000000     C* SE I PARAMETRI SONO OK, LI MEMORIZZO E IMPOSTO FLAG PARAM.MODIF.
027100000000IF  2C     $ERR          IFEQ      '0'
027200000000     C                   Z-ADD     1             SWPOSI
027300000000     C                   MOVEL     VCCCOM        DEPCOM
027400000000E   2C                   ENDIF
027500000000E   1C                   ENDIF
027600000000     C*
027700000000     C                   ENDSR
027800000000     C*------------------------------------------------------------------------*
027900000000     C* CHKPAR - CONTROLLA I PARAMETRI DI INIZIO RICERCA
028000000000     C*------------------------------------------------------------------------*
028100000000     C     CHKPAR        BEGSR
028200000000     C*
028300000000     C* SE C'E' UN ERRORE NON EMETTO IL SUBFILE PER FARE POSIZIONARE IL
028400000000     C* CURSORE SUL CAMPO DEL CONTROL
028500000000IF  1C     $ERR          IFEQ      '1'
028600000000     C                   MOVEL     '0'           *IN43
028700000000X   1C                   ELSE
028800000000     C                   MOVEL     '1'           *IN43
028900000000E   1C                   ENDIF
029000000000     C*
029100000000     C                   ENDSR
029200000000     C*------------------------------------------------------------------------*
029300000000     C* SETFIL - POSIZIONAMENTO E PRIMA LETTURA RECORD TABELLE
029400000000     C*------------------------------------------------------------------------*
029500000000     C     SETFIL        BEGSR
029600000000     C*
029700000000     C                   MOVEL     '1'           *IN43
029800000000     C                   Z-ADD     1             $I
029900000000     C*
030000000000     C* IMPOSTA SWITCH DI FINE CARICAMENTO SFL (SFLEOF)
030100000000IF  1C     CIM($I)       IFEQ      *ZEROS
030200000000     C                   Z-ADD     1             SFLEOF
030300000000E   1C                   ENDIF
030400000000     C*
030500000000     C                   ENDSR
030600000000     C*------------------------------------------------------------------------*
030700000000     C* LETFIL - LETTURA SCHIERE DA CARICARE A VIDEO
030800000000     C*------------------------------------------------------------------------*
030900000000     C     LETFIL        BEGSR
031000000000     C*
031100000000     C                   ADD       1             $I
031200000000     C*
031300000000     C* IMPOSTA SWITCH DI FINE CARICAMENTO SFL (SFLEOF)
031400000000IF  1C     $I            IFGT      200
031500000000IF  1C     CIM($I)       OREQ      *ZEROS
031600000000     C                   Z-ADD     1             SFLEOF
031700000000E   1C                   ENDIF
031800000000     C*
031900000000     C                   ENDSR
032000000000     C*------------------------------------------------------------------------*
032100000000     C* IRESFL - IMPOSTA I CAMPI DEL RECORD DEL SFL DALLE SCHIERE MEMORIZZATE
032200000000     C*------------------------------------------------------------------------*
032300000000     C     IRESFL        BEGSR
032400000000     C*
032600000000     C                   MOVEL     *ALL'*'       DEPCMD
032700000000     C                   MOVEL     *BLANKS       DEPFCR
032800020130     C                   MOVEL     *BLANKS       DEPFDR
032900020620     C                   Z-ADD     D$DAAS        depAAS
033000020620     C                   Z-ADD     D$DMGS        depMGS
033100020620     C                   Z-ADD     D$DDSP        DATLET            8 0
033200001206     C*
033300001206     C* SE LANCIO IN SIMULAZIONE FORZA L'ANNO RICHIESTO PER LA DECODIFICA DELLE
033400001206     C* VOCI / TARIFFE / COMPETENZA DA DECODIFICARE
033500001206IF  1C     PARASI        IFGT      *ZEROS
033600001206     C                   MOVEL     PARASI        DATLET
033700001206E   1C                   ENDIF
033701040209     c                   movel     datlet        comdsp            8 0
033702040209     c                   move      00            comdsp
033800000000     C*
033900000000     C* CARICA SOLO SE C'E' IL CODICE IMPORTO
034000000000IF  1C     CIM($I)       IFGT      *ZEROS
034100000000     C*--------------------
034200000000     C* COMPETENZE/RICAVI --> E C E C T 0 0 F
034300000000     C*--------------------
034400000000IF  2C     TIM($I)       IFEQ      'C'
034500000000     C                   Z-ADD     DEPNBC        KCTNBC
034600000000     C                   Z-ADD     CIM($I)       KCTCMP
034700000000     C     KEYECT        SETLL     ECECT01L
034701060921     c                   do        *hival
034800000000     C     KEYECT        READE     ECECT01L                               99
034801060921     c                   if        *in99
034802060921     c                   leave
034803060921     c                   end
034804060921     c                   if        ectatb <>' '
034805060921     c                   iter
034806060921     c                   end
035200040209IF  5C     comdsp        IFGE      ECTDDC
035300040209     C     comdsp        ANDLE     ECTDSC
035500000000     C                   MOVEL     ECTCMD        DEPCMD
035600000000     C                   MOVEL     ECTFCR        DEPFCR
035700020130     C                   MOVEL     ECTFDR        DEPFDR
035701060921     C                   leave
036100000000E   4C                   ENDIF
036200000000E   3C                   ENDDO
036300000000     C*
036400000000X   2C                   ELSE
036500000000     C*
036600000000     C*--------------------
036700000000     C* VOCI --> E C E V T 0 0 F
036800000000     C*--------------------
036900000000     C                   Z-ADD     CIM($I)       KVTVOC
037000000000     C                   Z-ADD     PIM($I)       KVTPRG
037100000000     C     KEYEVT        SETLL     ECEVT01L
037101060921     c                   do        *hival
037200000000     C     KEYEVT        READE     ECEVT01L                               99
037201060921     c                   if        *in99
037202060921     c                   leave
037203060921     c                   end
037204060921     c                   if        evtatb <>' '
037205060921     c                   iter
037206060921     c                   end
037600001206IF  5C     DATLET        IFGE      EVTDDV
037700001206     C     DATLET        ANDLE     EVTDSV
037900000000     C                   MOVEL     EVTVOD        DEPCMD
038000000000     C                   MOVEL     'V'           DEPFCR
038001060921     C                   leave
038400000000E   4C                   ENDIF
038500000000E   3C                   ENDDO
038600000000     C*
038700000000E   2C                   ENDIF
038800000000     C*--------------------
038900000000     C* S U B F I L E
039000000000     C*--------------------
039100000000     C                   MOVEL     DEPFCR        VSCFCR
039201031212     c* decodifico flag data riferimento calcolo
039202031212     c                   clear                   vscfdr
039203031212     c* Per anagrafiche obsolete
039204031212     c                   if        datlet < 20040101
039205031212     c                   if        depfdr = '0'
039206031212     c                   eval      vscfdr = 'MM spedizione'
039207031212     c                   else
039208031212     c                   eval      vscfdr = 'MM successivo'
039209031212     c                   end
039210031212     c                   else
039211031212     c* dal 2004 il flag della data è decodificato con la tabella CO1
039212031212     C                   CLEAR                   TIBS02DS
039213031212     C                   CLEAR                   DCO1
039214031212     C                   EVAL      T02TLA = 'L'
039215031212     C                   MOVEL     'C'           T02MOD
039216031212     C                   MOVEL(P)  depfdr        T02KE1
039217031212     C                   MOVEL     'CO1'         T02COD
039218031212     C                   MOVEL     KNSIF         T02SIF
039219031212     C                   CALL      'TIBS02R'
039220031212     C                   PARM                    KPJBA
039221031212     C                   PARM                    TIBS02DS
039222031212     C                   IF        T02ERR = *BLANKS
039223031212     C                   MOVEL     T02UNI        DCO1
039224031212     C                   MOVEL(p)  §CO1DES       Vscfdr
039225031212     C                   ENDIF
039226031212E   1C                   ENDIF
039300000000     C                   Z-ADD     CIM($I)       VSCCMP
039400000000     C                   MOVEL     DEPCMD        VSCCMD
039500000000     C                   Z-ADD     IM($I)        VSCIMP
039600000000     C*
039700000000E   1C                   ENDIF
039800000000     C*
039900000000     C                   ENDSR
040000000000     C*------------------------------------------------------------------------*
040100000000     C* CARIMP - CARICA LE SCHIERE CON GLI IMPORTI DEI RICAVI E DELLE COMPETENZE
040200000000     C*------------------------------------------------------------------------*
040300000000     C     CARIMP        BEGSR
040400000000     C*
040500950405     C     1             DO        50            I
040600950405     C                   Z-ADD     *ZEROS        DSWCN
040700950405     C                   MOVE      DSWCA         CAN(I)
040800950405     C                   Z-ADD     *ZEROS        DSWIN
040900950405     C                   MOVE      DSWIA         IAN(I)
041000950405     C                   ENDDO
041100950405     C                   MOVEA     CAN           D$DCCD
041200950405     C                   MOVEA     IAN           D$DICD
041300950405     C                   MOVEA     CAN           D$DCVD
041400950405     C                   MOVEA     CAN           D$DPVD
041500950405     C                   MOVEA     IAN           D$DIVD
041600020620     C***                EXSR      APDTAQ
041700020620     C***                EXSR      ADDTAQ
041800000000     C                   Z-ADD     *ZEROS        $I
041900000000     C*
042000990812     C* CONTROLLO ESISTENZA BOLLA IN TITAS10F
042100020620     C***                SETOFF                                       010203
042200020620     C***                Z-ADD     PARAAS        KTAAAS
042300020620     C***                Z-ADD     PARLNP        KTALNP
042400020620     C***                Z-ADD     PARNRS        KTANRS
042500020620     C***                Z-ADD     PARNSP        KTANSP
042600020620     C***                MOVEL     PARTBL        KTATBL
042700020620     C***  KEYTAS        CHAIN     TITAS30C                           99
042800020620IF  2C***  *IN99         IFEQ      *ON
042900020620     C***                MOVEL     '1'           $ERR
043000020620     C***                MOVEL     '1'           $FINE
043100020620E   1C***                ENDIF
043200000000     C*
043300000000     C* TRASFERISCE LA BOLLA LETTA, NELL'IMMAGINE -DOPO- DELLA DS
043400000000IF  1C     $ERR          IFEQ      '0'
043500020620     C***                EXSR      CDDTAQ
043600000000     C*
043700000000     C* LANCIA IL PROGRAMMA
043800020620     C***                MOVEL     'I'           D$GSTS
043900020620     C***                MOVEL     'I'           D$GSTO
044000020620     C***                Z-ADD     UDATE         D$GDAT
044100020620     C***                TIME                    D$GTIM
044200020620     C***                MOVEL     'EDPRN'       D$GUSR
044300020620     C***                MOVEL     'TREC78R'     D$GPGF
044400020620     C***                MOVEL     'GAITRAPRV '  D$GLIB
044500020620     C***                MOVEL     'TRECD6R'     D$GPGS
044600020620     C***  *IN01         IFEQ      *ON
044700020620     C***                MOVEL     'TITAS00F'    D$GFIL
044800020620     C***                MOVEL     'TITAS00F'    D$GMBR
044900020620     C***                ENDIF
045000020620     C***  *IN02         IFEQ      *ON
045100020620     C***                MOVEL     'TITAS10F'    D$GFIL
045200020620     C***                MOVEL     'TITAS10F'    D$GMBR
045300020620     C***                ENDIF
045400020620     C***                MOVEL     'N'           D$GSDA
045500020620     C***                MOVEL     'N'           D$GSDC
045600020620     C***                MOVEL     'N'           D$GSDF
045700020620     C***                MOVEL     'N'           D$GSDO
045800020620     C***                MOVEL     'N'           D$GSDP
045900020620     C***                MOVEL     'N'           D$GSDR
046000020620     C***                MOVEL     'N'           D$GSDT
046100020620     C***                MOVEL     'N'           D$GCET
046200020620     C***                MOVEL     'N'           D$GWFM
046300020620     C***                MOVEL     'N'           D$GFIM
046400020620     C***                MOVEL     'N'           D$GFCE
046500030123     C***                MOVEL     'N'           D$GCOP
046600020620     C***                MOVEL     *BLANKS       D$GERR
046700020620     C***                MOVEL     *BLANKS       D$GMSG
046800020620     C***                Z-ADD     *ZEROS        D$GPRE
046900020620     C***                MOVEL     *BLANKS       D$GXXX
047000001206     C*
047100001206     C* IMPOSTA ALTRI DATI SE LANCIO IN SIMULAZIONE
047200001206IF  2C     PARASI        IFGT      *ZEROS
047300001206     C                   MOVEL     PARASI        D$GASI
047400001204     C                   MOVEL     'S'           D$GFSI
047500001204     C                   MOVEL     DSD$G         D$GXXX
047600001206E   2C                   ENDIF
047700001206     C*
047800020620     C***                MOVEL     D$PDS1        D$PRE1
047900020620     C***                MOVEL     D$PDS2        D$PRE2
048000020620     C***                MOVEL     D$DDS1        D$DRE1
048100020620     C***                MOVEL     D$DDS2        D$DRE2
048200020620     C                   MOVEL     D$PRE1        D$PDS1
048300020620     C                   MOVEL     D$PRE2        D$PDS2
048400020620     C                   MOVEL     D$DRE1        D$DDS1
048500020620     C                   MOVEL     D$DRE2        D$DDS2
048600000000     C                   SETOFF                                         98
048700950405     C                   CALL      'TRECD6R'                            98
048800960301     C                   PARM                    DS$ECO
048900950125IF  2C     *IN98         IFEQ      *ON
049000950125     C     D$GERR        ORNE      *BLANKS
049100950125     C                   MOVEL     D$GMSG        VIDMSG
049200950125     C                   ELSE
049300950125     C                   MOVEL     *BLANKS       VIDMSG
049400000000     C*--------------------
049500000000     C*  C O M P E T E N Z E
049600000000     C*--------------------
049700000000     C*
049800000000     C* TRASFERISCO CODICI E IMPORTI IN SCHIERE ALFANUMERICHE
049900000000     C                   MOVEA     D$DCCD        CAD
050000000000     C                   MOVEA     D$DICD        IAD
050100950125     C                   Z-ADD     *ZEROS        C$D
050200950125     C                   Z-ADD     *ZEROS        I$D
050300000000     C*
050400000000     C* TRASFERISCO LE SCHIERE ALFANUMERICHE IN QUELLE NUMERICHE
050500950125DO  3C     1             DO        50            J
050600950125     C                   MOVE      CAD(J)        DSNUC2
050700000000     C                   Z-ADD     DSNUC1        C$D(J)
050800950125     C                   MOVE      IAD(J)        DSNUI2
050900000000     C                   Z-ADD     DSNUI1        I$D(J)
051000950125E   3C                   ENDDO
051100950125     C*
051200000000     C* TRASFERISCO LE SCHIERE CARICATE IN QUELLE DA PORTARE A VIDEO
051300950125DO  3C     1             DO        50            J
051400950125IF  4C     C$D(J)        IFGT      *ZEROS
051500000207     C*---       I$D,J     ANDGT*ZEROS
051600000000     C                   ADD       1             $I
051700000000     C                   Z-ADD     C$D(J)        CIM($I)
051800000000     C                   MOVEL     'C'           TIM($I)
051900000000     C                   Z-ADD     *ZEROS        PIM($I)
052000000000     C                   Z-ADD     I$D(J)        IM($I)
052100950125E   4C                   ENDIF
052200950125E   3C                   ENDDO
052300000000     C*--------------------
052400000000     C* V O C I
052500000000     C*--------------------
052600000000     C*
052700000000     C* TRASFERISCO CODICI E IMPORTI IN SCHIERE ALFANUMERICHE
052800000000     C                   MOVEA     D$DCVD        CAD
052900000000     C                   MOVEA     D$DPVD        PAD
053000000000     C                   MOVEA     D$DIVD        IAD
053100950125     C                   Z-ADD     *ZEROS        C$D
053200950125     C                   Z-ADD     *ZEROS        P$D
053300950125     C                   Z-ADD     *ZEROS        I$D
053400000000     C*
053500000000     C* TRASFERISCO LE SCHIERE ALFANUMERICHE IN QUELLE NUMERICHE
053600950125DO  3C     1             DO        50            J
053700950125     C                   MOVE      CAD(J)        DSNUC2
053800000000     C                   Z-ADD     DSNUC1        C$D(J)
053900950125     C                   MOVE      IAD(J)        DSNUI2
054000000000     C                   Z-ADD     DSNUI1        I$D(J)
054100950125     C                   MOVE      PAD(J)        DSNUC2
054200000000     C                   Z-ADD     DSNUC1        P$D(J)
054300950125E   3C                   ENDDO
054400000000     C*
054500000000     C* TRASFERISCO LE SCHIERE CARICATE IN QUELLE DA PORTARE A VIDEO
054600950125DO  3C     1             DO        50            J
054700950125IF  4C     C$D(J)        IFGT      *ZEROS
054800000000     C     P$D(J)        ANDGT     *ZEROS
054900000000     C     I$D(J)        ANDGT     *ZEROS
055000000000     C                   ADD       1             $I
055100000000     C                   Z-ADD     C$D(J)        CIM($I)
055200000000     C                   MOVEL     'V'           TIM($I)
055300000000     C                   Z-ADD     P$D(J)        PIM($I)
055400000000     C                   Z-ADD     I$D(J)        IM($I)
055500950125E   4C                   ENDIF
055600950125E   3C                   ENDDO
055700000000     C*
055800950125E   2C                   ENDIF
055900000000E   1C                   ENDIF
056000000000     C*
056100000000     C                   ENDSR
056200000000     C*------------------------------------------------------------------------*
056300000000     C* CARTAB - CARICA TABELLE
056400000000     C*------------------------------------------------------------------------*
056500000000     C     CARTAB        BEGSR
056600000000     C*--------------------
056700000000     C* NUMERO BLOCCO COMPETENZE
056800000000     C*--------------------
056900020923     C                   Z-ADD     1             KTBKUT
057000980130     C                   MOVEL     '2K'          KTBCOD
057100000000     C                   MOVEL     *BLANKS       KTBKEY
057200000000     C     KEYTAB        CHAIN     TABEL00F                           99
057300980130     C                   MOVEL     TBLUNI        DS2K
057400980130     C                   Z-ADD     §2KNBC        DEPNBC
057500000000     C*
057600000000     C                   ENDSR
057700020923     C*-------------------------------------------------------------------------
057800020923     C* REPERISCE I DATI UTENTE
057900020923     C*-------------------------------------------------------------------------
058000020923     C     REPDATIUTE    BEGSR
058100020923     C*
058200020923     C* INIZIALIZZA VARIABILI DI WRK
058300020923     C                   CLEAR                   TIBS34DS
058400020923     C                   CLEAR                   AZUTEDS
058500020923     C                   CLEAR                   DDATIUTE
058600020923     C*
058700020923     C     *DTAARA       DEFINE    §azute        azuteds
058800020923     C     *DTAARA       DEFINE    §datiute      ddatiute
058900020923     C                   IN(E)     *DTAARA
059000020923if  1C                   IF        %Error
059100020923     c                   EVAL      I34Tla = 'L'
059200020923     C                   CALL      'TIBS34R'
059300020923     C                   PARM                    Tibs34Ds
059400020923     C                   IN        *DTAARA
059500020923e   1C                   ENDIF
059600020923     C*
059700020923     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
059800020923     C                   MOVEL(P)  RSUT          DSFIRS           20
059900020923     C*
060000020923     C                   ENDSR
060100000000     C*------------------------------------------------------------------------*
060200000000     C* *INZSR - ROUTINE INIZIALE
060300000000     C*------------------------------------------------------------------------*
060400000000     C     *INZSR        BEGSR
060500000000     C*--------------------
060600000000     C* RICEVIMENTO PARAMETRI
060700000000     C*--------------------
060800000000     C     *ENTRY        PLIST
060801031212     C                   PARM                    KPJBA
060900020620     C                   PARM                    DS$ECO
061000020620     C                   PARM                    PARASI            4 0
061200020620     C***                MOVEL     KPJBU         PARAM
061300000000     C*--------------------
061400000000     C* CAMPI RIFERITI AL DATA BASE
061500000000     C*--------------------
061600020620     C***  *LIKE         DEFINE    TASAAS        KTAAAS
061700020620     C***  *LIKE         DEFINE    TASLNP        KTALNP
061800020620     C***  *LIKE         DEFINE    TASNRS        KTANRS
061900020620     C***  *LIKE         DEFINE    TASNSP        KTANSP
062000020620     C***  *LIKE         DEFINE    TASTBL        KTATBL
062100000000     C     *LIKE         DEFINE    ECTNBC        KCTNBC
062200000000     C     *LIKE         DEFINE    ECTCMP        KCTCMP
062300000000     C     *LIKE         DEFINE    EVTVOC        KVTVOC
062400000000     C     *LIKE         DEFINE    EVTPRG        KVTPRG
062500000000     C     *LIKE         DEFINE    TBLKUT        KTBKUT
062600000000     C     *LIKE         DEFINE    TBLCOD        KTBCOD
062700000000     C     *LIKE         DEFINE    TBLKEY        KTBKEY
062800000000     C     *LIKE         DEFINE    ECTCMD        DEPCMD
062900000000     C     *LIKE         DEFINE    ECTNBC        DEPNBC
063000000000     C     *LIKE         DEFINE    ECTFCR        DEPFCR
063100020130     C     *LIKE         DEFINE    ECTFDR        DEPFDR
063200000000     C*--------------------
063300000000     C* CAMPI NON RIFERITI AL DATA BASE
063400000000     C*--------------------
063500000000     C                   MOVEL     *BLANKS       VCCCOM            1
063600000000     C                   MOVEL     *BLANKS       DEPCOM            1
063700000000     C                   MOVEL     *BLANKS       DEPFCR
063800020130     C                   MOVEL     *BLANKS       DEPFDR
063900000000     C                   Z-ADD     *ZEROS        DEPNBC
064000000000     C                   Z-ADD     *ZEROS        J                 4 0
064100000000     C                   Z-ADD     *ZEROS        $I                4 0
064300000000     C*--------------------
064400000000     C* K L I S T
064500000000     C*--------------------
064600990812     C* CHIAVE LETTURA TITAS30C
064700020620     C***  KEYTAS        KLIST
064800020620     C***                KFLD                    KTAAAS
064900020620     C***                KFLD                    KTALNP
065000020620     C***                KFLD                    KTANRS
065100020620     C***                KFLD                    KTANSP
065200020620     C***                KFLD                    KTATBL
065300000000     C*
065400000000     C* CHIAVE LETTURA ECECT01L - PARZIALE
065500000000     C     KEYECT        KLIST
065600000000     C                   KFLD                    KCTNBC
065700000000     C                   KFLD                    KCTCMP
065800000000     C*
065900000000     C* CHIAVE LETTURA ECEVT01L - COMPLETA
066000000000     C     KEYEVT        KLIST
066100000000     C                   KFLD                    KVTVOC
066200000000     C                   KFLD                    KVTPRG
066300000000     C*
066400000000     C* CHIAVE LETTURA TABEL00F - COMPLETA
066500000000     C     KEYTAB        KLIST
066600000000     C                   KFLD                    KTBKUT
066700000000     C                   KFLD                    KTBCOD
066800000000     C                   KFLD                    KTBKEY
066900000000     C*
067000000000     C* ESEGUE OPERAZIONI INIZIALI STANDARD
067100000000     C                   EXSR      INZSTD
067200000000     C*
067300000000     C* CARICA TABELLE
067400000000     C                   EXSR      CARTAB
067500000000     C*
067600000000     C* CARICA LE SCHIERE CON GLI IMPORTI DEI RICAVI E DELLE COMPETENZE
067700000000     C                   EXSR      CARIMP
067800000000     C*
067900000000     C* ESEGUE OPERAZIONI INIZIALI STANDARD PER SUBFILE
068000000000     C                   EXSR      INZSFL
068100000000     C*
068200000000     C* IMPOSTO CAMPI TESTATA VIDEO E CONTROL FISSI
068300000000     C                   MOVEL     DSFIRS        VTDRSU
068400020620     C***                Z-ADD     PARAAS        VCCAAS
068500020620     C***                Z-ADD     PARLNP        VCCLNP
068600020620     C***                Z-ADD     PARNRS        VCCNRS
068700020620     C***                Z-ADD     PARNSP        VCCNSP
068800020620     C***                MOVEL     PARTBL        VCCTBL
068900020620     C                   Z-ADD     D$DAAS        VCCAAS
069000020620     C                   Z-ADD     D$DLNP        VCCLNP
069100020620     C                   Z-ADD     D$DNRS        VCCNRS
069200020620     C                   Z-ADD     D$DNSP        VCCNSP
069300020620     C                   MOVEL     D$DTBL        VCCTBL
069400020620     C                   Z-ADD     PARASI        VCCASI
069500000000     C*
069600000000     C* EMETTO VIDEATA SUBFILE
069700000000     C                   EXSR      CARVS
069800000000     C*
069900000000     C                   ENDSR
070000000000     C*------------------------------------------------------------------------*
070100990812     C* ROUTINE GENERALIZZATA PASSAGGIO IMMAGINE BOLLA DI TITAS10F
070200000000     C*------------------------------------------------------------------------*
070300020620      ***/COPY GAITRASRC/SRCTREC,TRECDT
070400000000     C*------------------------------------------------------------------------*
070500000000     C*    §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
070600000000     C*    §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
070700000000     C*    LA PARTE DI SPECIFICHE 'C' SOTTOSTANTE SI RIFA' A PARTICOLARI
070800000000     C*    STANDARDS DI PROGRAMMAZIONE E NELLA MAGGIOR PARTE DEI CASI
070900000000     C*    NON E' NECESSARIO MODIFICARLA, NEL CASO FARE ATTENZIONE.
071000000000     C*    §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
071100000000     C*    §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
071200000000     C*------------------------------------------------------------------------*
071300000000     C* INZSTD - ROUTINE INIZIALE STANDARD
071400000000     C*------------------------------------------------------------------------*
071500000000     C     INZSTD        BEGSR
071600000000     C* CHIAMA ROUTINE PER RIEMPIRE LA DS DSKEY (TASTI PREMUTI)
071700000000     C                   CALL      'XKEYS'
071800000000     C                   PARM                    DSKEYS
071900000000     C                   MOVEA     KEYRIG        KEYS
072000000000     C* PARAMETRI PER RICHIAMO PGM (ROUTINE) XMSG (MESSAGGI)
072100000000     C     XMSGPA        PLIST
072200000000     C                   PARM                    DSMSMS
072300000000     C                   PARM                    DSMSPG
072400000000     C                   PARM                    DSMSKE
072500000000     C                   PARM                    DSMSER
072600000000     C* IMPOSTA PER CL=XMSG (MESSAGGI ALLA CODA PGM), FISSI IN TUTTO IL PGM
072700000000     C                   MOVEL     'TREC79R'     DSMSPG
072800000000     C                   MOVEL     'TREC79R'     VMCQUE
072900000000     C                   MOVEL     *BLANKS       VMCKEY
073000000000     C                   MOVEL     *BLANKS       DSMSMS
073100000000     C* INIZIALIZZO FLAG CONTROLLO PGM
073200000000     C                   MOVEL     '0'           $ERR              1
073300000000     C                   MOVEL     '0'           $F03              1
073400000000     C                   MOVEL     *BLANKS       $GES              2
073500000000     C                   MOVEL     *BLANKS       $MSG             80
073600000000     C                   MOVEL     '0'           $FINE             1
073700000000     C                   MOVEL     '0'           $TESVD            1
073800000000     C                   MOVEL     'G'           $TGES             1
073900000000     C* LANCIO ROUTINE PER VERIFICARE SE
074000000000     C* SONO IN SEDE, IN FILIALE DI 1° LIVELLO O IN FILIALE DI 2° LIVELLO
074100000000     C* E PER RECUPERARE LA RAGIONE SOCIALE DELL'UTENTE
074200020923     C                   EXSR      REPDATIUTE
074300000000     C* POSIZIONE MINIMA E MASSIMA DEL CURSORE PER GESTIONE MESSAGGI
074400000000     C                   BITOFF    '01234567'    POSMI1            1
074500000000     C                   BITOFF    '01234567'    POSMI2            1
074600000000     C                   BITOFF    '01234567'    POSMA1            1
074700000000     C                   BITOFF    '01234567'    POSMA2            1
074800000000     C                   BITON     '34'          POSMI1            1
074900000000     C                   BITON     '7'           POSMI2            1
075000000000     C                   BITON     '34'          POSMA1            1
075100000000     C                   BITON     '0'           POSMA2            1
075200000000     C                   MOVE      POSMI2        POSMIN            2
075300000000     C                   MOVEL     POSMI1        POSMIN
075400000000     C                   MOVE      POSMA2        POSMAX            2
075500000000     C                   MOVEL     POSMA1        POSMAX
075600000000     C                   ENDSR
075700000000     C*------------------------------------------------------------------------*
075800000000     C* FUNSTD - LANCIA SUBROUTINE STANDARD
075900000000     C*------------------------------------------------------------------------*
076000000000     C     FUNSTD        BEGSR
076100000000     C                   SELECT
076200000000     C     KEY           WHENEQ    F3
076300000000     C                   EXSR      F03STD
076400000000     C     KEY           WHENEQ    F12
076500000000     C                   EXSR      F12STD
076600000000     C     KEY           WHENEQ    F24
076700000000     C                   EXSR      F24STD
076800000000     C                   OTHER
076900000000     C                   MOVEL     MSG(2)        $MSG
077000000000     C                   EXSR      CARMSG
077100000000     C                   ENDSL
077200000000     C                   ENDSR
077300000000     C*------------------------------------------------------------------------*
077400000000     C* F03STD -GEST.TASTO FUNZ. F03 STANDARD ===> FINE
077500000000     C*------------------------------------------------------------------------*
077600000000     C     F03STD        BEGSR
077700000000     C* IMPOSTO FLAG DI PREMUTO TASTO F03 (A LIVELLO PROCEDURA)
077800000000     C                   MOVEL     '1'           $F03
077900000000     C* IMPOSTO FLAG DI FINE PROGRAMMA
078000000000     C                   MOVEL     '1'           $FINE
078100000000     C                   ENDSR
078200000000     C*------------------------------------------------------------------------*
078300000000     C* F12STD -GEST.TASTO FUNZ. F12 STANDARD ===> RITORNO
078400000000     C*------------------------------------------------------------------------*
078500000000     C     F12STD        BEGSR
078600000000     C* IMPOSTO FLAG DI FIN PGM
078700000000     C                   MOVEL     '1'           $FINE
078800000000     C                   ENDSR
078900000000     C*------------------------------------------------------------------------*
079000000000     C* F24STD -GEST.TASTO FUNZ. F24 STANDARD ===> ALTRI TASTI
079100000000     C*------------------------------------------------------------------------*
079200000000     C     F24STD        BEGSR
079300000000     C* GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI
079400000000     C                   EXSR      OUTFUN
079500000000     C                   ENDSR
079600000000     C*------------------------------------------------------------------------*
079700000000     C* INZSFL - ROUTINE INIZIALE STANDARD PER SUBFILE
079800000000     C*------------------------------------------------------------------------*
079900000000     C     INZSFL        BEGSR
080000000000     C* DEFINISCE LE VARIABILI DI PROGRAMMA PER IL SUBFILE
080100000000     C     *LIKE         DEFINE    VCCNBR        SFLPAG
080200000000     C     *LIKE         DEFINE    VCCNBR        SFLEOF
080300000000     C     *LIKE         DEFINE    VCCNBR        SFLERR
080400000000     C     *LIKE         DEFINE    VCCNBR        SFLULT
080500000000     C     *LIKE         DEFINE    VCCNBR        WFLULT
080600000000     C     *LIKE         DEFINE    VCCNBR        SFLRIG
080700000000     C* INIZIALIZZO CONTATORE DI NUMERO RIGHE PER PAGINA
080800000000     C* (DEVE ESSERE UGUALE A SFLPAG (IN RECORD CONTROLLO SFL)
080900000000     C                   Z-ADD     12            SFLPAG
081000000000     C                   ENDSR
081100000000     C*------------------------------------------------------------------------*
081200000000     C* FUNSFL - LANCIA SUBROUTINE STANDARD SUBFILE
081300000000     C*------------------------------------------------------------------------*
081400000000     C     FUNSFL        BEGSR
081500000000     C                   SELECT
081600000000     C     KEY           WHENEQ    ENTER
081700000000     C                   EXSR      ENTSFL
081800000000     C     KEY           WHENEQ    F3
081900000000     C                   EXSR      F03STD
082000000000     C     KEY           WHENEQ    F5
082100000000     C                   EXSR      F05STD
082200000000     C     KEY           WHENEQ    F12
082300000000     C                   EXSR      F12STD
082400000000     C     KEY           WHENEQ    F17
082500000000     C                   EXSR      F17STD
082600000000     C     KEY           WHENEQ    F18
082700000000     C                   EXSR      F18STD
082800000000     C     KEY           WHENEQ    F23
082900000000     C                   EXSR      F23STD
083000000000     C     KEY           WHENEQ    F24
083100000000     C                   EXSR      F24STD
083200000000     C     KEY           WHENEQ    ROLLUP
083300000000     C                   EXSR      RUPSTD
083400000000     C     KEY           WHENEQ    ROLLDN
083500000000     C                   EXSR      RDNSTD
083600000000     C                   OTHER
083700000000     C                   MOVEL     MSG(2)        $MSG
083800000000     C                   EXSR      CARMSG
083900000000     C                   ENDSL
084000000000     C                   ENDSR
084100000000     C*------------------------------------------------------------------------*
084200000000     C* F05STD -GEST.TASTO FUNZ. F05 STANDARD ===> RIVISUALIZZAZIONE
084300000000     C*------------------------------------------------------------------------*
084400000000     C     F05STD        BEGSR
084500000000     C* LANCIA LA ROUTINE DI CARICAMENTO DEL SUBFILE
084600000000     C                   EXSR      POSSFL
084700000000     C                   ENDSR
084800000000     C*------------------------------------------------------------------------*
084900000000     C* F17STD -GEST.TASTO FUNZ. F17 STANDARD ===> INIZIO DATI
085000000000     C*------------------------------------------------------------------------*
085100000000     C     F17STD        BEGSR
085200000000     C* IMPOSTA SFLRCDNBR PER PRIMA PAGINA
085300000000     C                   Z-ADD     1             VCCNBR
085400000000     C                   ENDSR
085500000000     C*------------------------------------------------------------------------*
085600000000     C* F18STD -GEST.TASTO FUNZ. F18 STANDARD ===> FINE DATI
085700000000     C*------------------------------------------------------------------------*
085800000000     C     F18STD        BEGSR
085900000000     C* IMPOSTA SFLRCDNBR PER ULTIMA PAGINA
086000000000     C                   Z-ADD     SFLULT        VCCNBR
086100000000     C                   ENDSR
086200000000     C*------------------------------------------------------------------------*
086300000000     C* F23STD -GEST.TASTO FUNZ. F23 STANDARD ===> ALTRE OPZIONI
086400000000     C*------------------------------------------------------------------------*
086500000000     C     F23STD        BEGSR
086600000000     C* GESTIONE VISUALIZZAZIONE OPZIONI
086700000000     C                   EXSR      OUTOPZ
086800000000     C                   ENDSR
086900000000     C*------------------------------------------------------------------------*
087000000000     C* RUPSTD -GEST.TASTO FUNZ. ROLLUP STANDRAD ===> PAGINA SUCCESSIVA
087100000000     C*------------------------------------------------------------------------*
087200000000     C     RUPSTD        BEGSR
087300000000     C* SE IL CURSORE E' SULLA RIGA 24 SIGNIFICA CHE E' RICHIESTO IL ROLLUP
087400000000     C* SUL SUBFILE MESSAGGI QUINDI NON ESEGUO RULLUP SUL SUBFILE DATI
087500000000     C     CURSOR        IFGE      POSMIN
087600000000     C     CURSOR        ANDLE     POSMAX
087700000000     C                   ELSE
087800000000     C                   EXSR      CARSFL
087900000000     C                   ENDIF
088000000000     C                   ENDSR
088100000000     C*------------------------------------------------------------------------*
088200000000     C* RDNSTD -GEST.TASTO FUNZ. ROLLDOWN STANDRAD ===> PAGINA PRECEDENTE
088300000000     C*------------------------------------------------------------------------*
088400000000     C     RDNSTD        BEGSR
088500000000     C* SE IL CURSORE E' SULLA RIGA 24 SIGNIFICA CHE E' RICHIESTO IL ROLLDOWN
088600000000     C* SUL SUBFILE MESSAGGI QUINDI NON ESEGUO RULLDOWN SUL SUBFILE DATI
088700000000     C     CURSOR        IFGE      POSMIN
088800000000     C     CURSOR        ANDLE     POSMAX
088900000000     C                   ELSE
089000000000     C* INVIO MESSAGGIO DI INIZIO ELENCO
089100000000     C                   MOVEL     '1'           $ERR
089200000000     C                   MOVEL     MSG(5)        $MSG
089300000000     C                   EXSR      CARMSG
089400000000     C                   ENDIF
089500000000     C                   ENDSR
089600000000     C*------------------------------------------------------------------------*
089700000000     C* ENTSFL - GEST.TASTO FUNZ. ENTER CON SUBFILE ===> INVIO
089800000000     C*------------------------------------------------------------------------*
089900000000     C     ENTSFL        BEGSR
090000000000     C* CONTROLLO SE RICHIESTO NUOVO POSIZIONAMENTO FILE DA CARICARE
090100000000     C                   EXSR      NEWPOS
090200000000     C* SE I PARAMETRI DI POSIZIONAMENTO SONO ERRATI NON RIEMETTE IL SUBFILE
090300000000     C     $ERR          IFEQ      '0'
090400000000     C* SWPOSI: 0=NO NEW POSIZION.   1=NEW POSIZION.
090500000000     C     SWPOSI        IFNE      0
090600000000     C                   EXSR      POSSFL
090700000000     C                   ELSE
090800000000     C* GESTIONE SFL CON TASTO FUNZIONALE ENTER
090900000000     C     SFLULT        IFNE      0
091000000000     C                   EXSR      OPZSFL
091100000000     C* SWMODI: 0=NON RICARICO IL SUBFILE   1=RICARICO IL SUBFILE
091200000000     C     SWMODI        IFNE      0
091300000000     C                   EXSR      POSSFL
091400000000     C                   ENDIF
091500000000     C                   ENDIF
091600000000     C                   ENDIF
091700000000     C                   ENDIF
091800000000     C                   ENDSR
091900000000     C*------------------------------------------------------------------------*
092000000000     C* POSSFL - RICARICAMENTO SFL DA NUOVO POSIZIONAMENTO FILE DA CARICARE
092100000000     C*------------------------------------------------------------------------*
092200000000     C     POSSFL        BEGSR
092300000000     C* CLEAR SFL (ELIMINA CONTENUTO SFL)
092400000000     C                   EXSR      CLRSFL
092500000000     C* CONTROLLO SE VI SONO REC. CHE SODDISFANO I PARAMETRI IN INPUT
092600000000     C                   EXSR      SETFIL
092700000000     C* SE ESISTONO RECORD VALIDI CARICO IL SUBFILE
092800000000     C     SFLEOF        IFEQ      0
092900000000     C                   EXSR      CARSFL
093000000000     C                   ENDIF
093100000000     C                   ENDSR
093200000000     C*------------------------------------------------------------------------*
093300000000     C* OPZSFL - GESTIONE OPZIONE SFL CON TASTO FUNZIONALE ENTER
093400000000     C*------------------------------------------------------------------------*
093500000000     C     OPZSFL        BEGSR
093600000000     C* AZZERO SWITCH DI ERRORI IN RECORD SFL (OPZOK: N=ERRORE S=OK)
093700000000     C                   MOVEL     'S'           OPZOK             1
093800000000     C* AZZERO CONTATORE DI RIGHE SFL VARIATE
093900000000     C                   Z-ADD     0             SFLVAR            5 0
094000000000     C* INIZIALIZZO COMODO CON PRIMO RECORD SFL IN ERRORE
094100000000     C                   Z-ADD     0             SFLERR
094200000000     C* SALVO ULTIMO RECORD SCRITTO IN SFL IN COMODO
094300000000     C                   Z-ADD     SFLULT        WFLULT
094400000000     C* SCORRIMENTO DEI SOLI RECORD VARIATI NEL SFL
094500000000     C                   READC     EC79DVS                                49
094600000000     C     *IN49         DOWEQ     '0'
094700000000     C* ADD CONTATORE DI RIGHE SFL VARIATE
094800000000     C                   ADD       1             SFLVAR
094900000000     C* SE RIGHE PRECEDENTI SENZA ERRORI (OPZOK: N=ERRORE S=OK)
095000000000     C     OPZOK         IFEQ      'S'
095100000000     C* CONTROLLO ED ESECUZIONE OPZIONI RICHIESTE (OPZOK: N=ERRORE S=OK)
095200000000     C     VSCOPZ        IFNE      *BLANKS
095300000000     C                   EXSR      CONOPZ
095400000000     C     OPZOK         IFEQ      'S'
095500000000     C                   EXSR      GESOPZ
095600000000     C                   ENDIF
095700000000     C                   ENDIF
095800000000     C* SE OK    : 1. AZZERO SFLNXTCHG PER RECORD SFL
095900000000     C*            2. PULIZIA SCELTA IN SFL
096000000000     C     OPZOK         IFEQ      'S'
096100000000     C                   MOVEL     '0'           *IN41
096200000000     C                   MOVEL     *BLANKS       VSCOPZ
096300000000     C                   ELSE
096400000000     C* SE NON OK: 1. ACCENDO SFLNXTCHG COSI' COSTRINGO ALLA CORREZIONE
096500000000     C*            2. IMPOSTO RECORD CON PRIMO ERRORE
096600000000     C                   MOVEL     '1'           *IN41
096700000000     C                   Z-ADD     SFLULT        SFLERR
096800000000     C                   ENDIF
096900000000     C                   ELSE
097000000000     C* SE RIGHE PRECEDENTI CON ERRORI (SFLERR DIVERSO DA 0):
097100000000     C*   . ACCENDO SFLNXTCHG COSI' RIPROPONE LA RIGA IL PROSSIMO GIRO
097200000000     C                   MOVEL     '1'           *IN41
097300000000     C                   ENDIF
097400000000     C* IMPOSTO RIGA SFL SUL QUALE POSIZIONARSI:
097500000000     C*   . SE ERRORE        MI POSIZIONO SULLA PRIMA RIGA IN ERRORE
097600000000     C*   . SE NESSUN ERRORE MI POSIZIONO SULL'ULTIMA RIGA VARIATA
097700000000     C     OPZOK         IFNE      'S'
097800000000     C                   Z-ADD     SFLERR        VCCNBR
097900000000     C                   ELSE
098000000000     C                   Z-ADD     SFLULT        VCCNBR
098100000000     C                   ENDIF
098200000000     C* AGGIORNO RECORD SFL
098300000000     C                   UPDATE    EC79DVS
098400000000     C                   READC     EC79DVS                                49
098500000000     C                   ENDDO
098600000000     C* REIMPOSTO ULTIMO RECORD SCRITTO IN SFL DA COMODO
098700000000     C                   Z-ADD     WFLULT        SFLULT
098800000000     C* SE NESSUNA RIGA VARIATA, CON ENTER SI RITORNA AL PGM CHIAMANTE
098900000000     C     SFLVAR        IFEQ      0
099000000000     C                   MOVEL     '1'           $FINE
099100000000     C                   ENDIF
099200000000     C                   ENDSR
099300000000     C*------------------------------------------------------------------------*
099400000000     C* CLRSFL - ELIMINAZIONE CONTENUTO SFL
099500000000     C*------------------------------------------------------------------------*
099600000000     C     CLRSFL        BEGSR
099700000000     C* INIZIALIZZO SWITCH DI FINE FILE "FISICO"
099800000000     C* (0=NON FINE FILE  1=FINE FILE)
099900000000     C                   Z-ADD     0             SFLEOF
100000000000     C* INIZIALIZZO CONTATORI DI ULTIMO RECORD IN SFL
100100000000     C                   Z-ADD     0             SFLULT
100200000000     C* ACCENSIONE INDICATORE PER CLEAR SFL
100300000000     C                   MOVEL     '1'           *IN40
100400000000     C                   WRITE     EC79DVC
100500000000     C                   MOVEL     '0'           *IN40
100600000000     C                   ENDSR
100700000000     C*------------------------------------------------------------------------*
100800000000     C* CARSFL - CARICAMENTO SFL
100900000000     C*------------------------------------------------------------------------*
101000000000     C     CARSFL        BEGSR
101100000000     C* SE IL SFL E' VUOTO INIZIO LETTURA DA POSIZ. RICHIESTA
101200000000     C     SFLULT        IFEQ      0
101300000000     C                   EXSR      SETFIL
101400000000     C                   ENDIF
101500000000     C* SALVO SFLRCDNBR PER CONFRONTI SUCCESSIVI
101600000000     C                   Z-ADD     SFLULT        WFLULT
101700000000     C* CICLO:
101800000000     C*    -CARICO NUMERO DI RIGHE=SFLPAG
101900000000     C*    -SFLEOF=0 (NON FINE FILE "FISICO")
102000000000     C                   Z-ADD     1             SFLRIG
102100000000     C     SFLRIG        DOWLE     SFLPAG
102200000000     C     SFLEOF        ANDEQ     0
102300000000     C* IMPOSTO CAMPI DEL RECORD SFL SOLO SE RECORD E' OK CIOE' ...
102400000000     C                   MOVEL     *BLANKS       VSCOPZ
102500000000     C* IMPOSTA I CAMPI DEL RECORD DEL SFL DA FILE DA CARICARE
102600000000     C                   EXSR      IRESFL
102700000000     C* AGGIORNA CHIAVE PER RECORD SFL E SFLRCDNBR
102800000000     C                   ADD       1             SFLULT
102900000000     C* SCRITTURA RECORD SFL
103000000000     C                   WRITE     EC79DVS
103100000000     C                   ADD       1             SFLRIG
103200000000     C* LETTURA FILE DA CARICARE
103300000000     C                   EXSR      LETFIL
103400000000     C                   ENDDO
103500000000     C* IMPOSTO SFLRCDNBR DELLA SUCCESSIVA PAGINA SE CARICATI ALTRI RECORD
103600000000     C     WFLULT        IFNE      SFLULT
103700000000     C                   Z-ADD     WFLULT        VCCNBR
103800000000     C                   ADD       1             VCCNBR
103900000000     C                   ENDIF
104000000000     C* SE USCITO DAL LOOP CON FINE FILE MANDO MESSAGGIO DI FINE ELENCO
104100000000     C     SFLEOF        IFEQ      1
104200000000     C     SFLULT        ANDGT     *ZEROS
104300000000     C                   MOVEL     '1'           $ERR
104400000000     C                   MOVEL     MSG(6)        $MSG
104500000000     C                   EXSR      CARMSG
104600000000     C* AGGIORNO L'ULTIMO RECORD DEL SFL PER POSIZIONARE IL CURSORE
104700000000     C     SFLULT        CHAIN     EC79DVS                            99
104800000000     C     *IN99         IFEQ      '0'
104900000000     C                   SETON                                        45
105000000000     C                   UPDATE    EC79DVS
105100000000     C                   SETOFF                                       45
105200000000     C                   ENDIF
105300000000     C                   ENDIF
105400000000     C                   ENDSR
105500000000     C*------------------------------------------------------------------------*
105600000000     C* OUTSFL - EMISSIONE SUBFILE A VIDEO
105700000000     C*------------------------------------------------------------------------*
105800000000     C     OUTSFL        BEGSR
105900000000     C* SE IL N.DI MESSAGGIO E' > 0 LANCIO ROUTINE VISUALIZZAZIONE MESSAGGIO
106000000000     C     MSGULT        IFNE      *ZEROS
106100000000     C                   EXSR      OUTMSG
106200000000     C                   ENDIF
106300000000     C* GESTIONE AUTOMATICA SFLEND (+ A VIDEO)
106400000000     C* SFLEOF=0 NON RAGGIUNTO FINE FILE "FISICO"
106500000000     C     SFLEOF        IFEQ      0
106600000000     C                   MOVEL     '0'           *IN42
106700000000     C                   ELSE
106800000000     C* SFLEOF=1 RAGGIUNTO FINE FILE "FISICO"
106900000000     C                   MOVEL     '1'           *IN42
107000000000     C                   ENDIF
107100000000     C* EMISSIONE SUBFILE
107200000000     C     SFLULT        IFLE      0
107300000000     C*  --> CASO DI SFL VUOTO
107400000000     C                   MOVEL     '0'           *IN43
107500000000     C                   WRITE     EC79DVV
107600000000     C                   WRITE     EC79DVC
107700000000     C                   READ      EC79DVC                                99
107800000000     C                   ELSE
107900000000     C*  --> CASO DI SFL CON RECORDS
108000000000     C                   WRITE     EC79DVC
108100000000     C                   READ      EC79DVC                                99
108200000000     C                   ENDIF
108300000000     C* MEMORIZZA IL N.RECORD DELLA PRIMA RIGA PAG.CORRENTE SFL
108400000000     C* (NEL CASO VENGANO UTILIZZATI ROLLUP E ROLLDOWN PERDEREI IL REC.CORRENTE)
108500000000     C                   Z-ADD     FRTSFL        VCCNBR
108600000000     C                   ENDSR
108700000000     C*------------------------------------------------------------------------*
108800000000     C* CARMSG - CARICA MESSAGGIO NEL SUBFILE MESSAGGI E LO INVIA ALLA
108900000000     C*          CODA MESSAGGI DEL PGM
109000000000     C*------------------------------------------------------------------------*
109100000000     C     CARMSG        BEGSR
109200000000     C* SE QUELLO CHE STO PER CARICARE E' IL PRIMO MESSAGGIO
109300000000     C* PULISCO SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
109400000000     C     MSGULT        IFEQ      0
109500000000     C                   MOVEL     '1'           *IN40
109600000000     C                   WRITE     EC79DVK
109700000000     C                   MOVEL     '0'           *IN40
109800000000     C                   ENDIF
109900000000     C* INVIA MESSAGGIO ALLA CODA DEL PGM
110000000000     C                   MOVEL     $MSG          DSMSMS
110100000000     C                   CALL      'XMSG'        XMSGPA
110200000000     C     DSMSER        IFEQ      0
110300000000     C                   MOVEL     DSMSKE        VMCKEY
110400000000     C                   ENDIF
110500000000     C* CARICA IL MESSAGGIO NEL SUBFILE MESSAGGI
110600000000     C                   ADD       1             MSGULT            4 0
110700000000     C                   WRITE     EC79DVM
110800000000     C* PULISCO CAMPO MESSAGGIO APPENA CARICATO
110900000000     C                   MOVEL     *BLANKS       $MSG
111000000000     C                   ENDSR
111100000000     C*------------------------------------------------------------------------*
111200000000     C* OUTMSG - EMETTE A VIDEO IL SUBFILE MESSAGGI
111300000000     C*------------------------------------------------------------------------*
111400000000     C     OUTMSG        BEGSR
111500000000     C* VISUALIZZAZIONE MESSAGGIO
111600000000     C                   MOVEL     '1'           *IN44
111700000000     C                   WRITE     EC79DVK
111800000000     C* AZZERO LA CHIAVE DEL SUBFILE MESSAGGI
111900000000     C                   Z-ADD     *ZEROS        MSGULT
112000000000     C                   ENDSR
112100000000     C*------------------------------------------------------------------------*
112200000000     C* CARFUN - CARICA LE SCHIERE PER LA GESTIONE DEI TASTI FUNZIONALI
112300000000     C*          INPUT:  - FUN    (SCHIERA TASTI ABILITATI PER IL VIDEO ATTUALE)
112400000000     C*          OUTPUT: - FFU    (SCHIERA FUNZIONI)
112500000000     C*                  - FDE    (SCHIERA DESCRIZIONI FUNZIONI)
112600000000     C*                  - FAT    (SCHIERA FLAG DI FUNZIONE ATTIVA S/N)
112700000000     C*------------------------------------------------------------------------*
112800000000     C     CARFUN        BEGSR
112900000000     C                   Z-ADD     *ZEROS        FUNULT            5 0
113000000000     C     1             DO        31            I                 5 0
113100000000     C                   MOVEL     FUN(I)        FUNRIG
113200000000     C                   MOVEL     FUNFUN        FFU(I)
113300000000     C                   MOVEL     FUNDES        FDE(I)
113400020923     C     DUTLPO        IFEQ      'S'
113500000000     C     $TGES         IFEQ      'G'
113600000000     C                   MOVEL     FUNASG        FAT(I)
113700000000     C                   ELSE
113800000000     C                   MOVEL     FUNASI        FAT(I)
113900000000     C                   ENDIF
114000000000     C                   ELSE
114100000000     C     $TGES         IFEQ      'G'
114200000000     C                   MOVEL     FUNAFG        FAT(I)
114300000000     C                   ELSE
114400000000     C                   MOVEL     FUNAFI        FAT(I)
114500000000     C                   ENDIF
114600000000     C                   ENDIF
114700000000     C     FAT(I)        IFEQ      'E'
114800000000     C                   MOVEL     USER          WEDP              3
114900000000     C     WEDP          IFEQ      'EDP'
115000000000     C                   MOVEL     'S'           FAT(I)
115100000000     C                   ELSE
115200000000     C                   MOVEL     'N'           FAT(I)
115300000000     C                   ENDIF
115400000000     C                   ENDIF
115500000000     C                   ENDDO
115600000000     C                   ENDSR
115700000000     C*------------------------------------------------------------------------*
115800000000     C* OUTFUN - EMETTE A VIDEO LE FUNZIONI ABILITATE
115900000000     C*          INPUT:  - FUNULT (ULTIMA FUNZIONE EMESSA NEL VIDEO PRECEDENTE)
116000000000     C*          OUTPUT: - VFD001-008 (CAMPI FUNZIONI A VIDEO)
116100000000     C*                  - FUNULT (ULTIMA FUNZIONE EMESSA A VIDEO)
116200000000     C*------------------------------------------------------------------------*
116300000000     C     OUTFUN        BEGSR
116400000000     C     1             DO        8             I
116500000000     C                   MOVEL     *BLANKS       FUV(I)
116600000000     C                   ENDDO
116700000000     C* CONTROLLO QUANTE FUNZIONI ATTIVE DA EMETTERE A VIDEO CI SONO
116800000000     C                   Z-ADD     *ZEROS        FUNNAT            2 0
116900000000     C     1             DO        31            I
117000000000     C     FAT(I)        IFEQ      'S'
117100000000     C     FDE(I)        ANDNE     *BLANKS
117200000000     C                   ADD       1             FUNNAT
117300000000     C                   ENDIF
117400000000     C                   ENDDO
117500000000     C* SE VI SONO PIU' DI 8 FUNZIONI ATTIVO IL TASTO F24
117600000000     C     FUNNAT        IFGT      8
117700000000     C                   MOVEL     'S'           FAT(24)
117800000000     C                   ENDIF
117900000000     C* IMPOSTO IL N.DI FUNZIONI VISUALIZZABILI A 8, E SE SONO ATTIVE F23 E F24
118000000000     C* (SE ATTIVE DA VISUALIZZARE SEMPRE) DIVENTANO 6
118100000000     C                   Z-ADD     8             FUNVIS            5 0
118200000000     C     FAT(23)       IFEQ      'S'
118300000000     C                   SUB       1             FUNVIS
118400000000     C                   ENDIF
118500000000     C     FAT(24)       IFEQ      'S'
118600000000     C                   SUB       1             FUNVIS
118700000000     C                   ENDIF
118800000000     C* AGGIUNGO UNO ALL'ULTIMA FUNZIONE VISUALIZZATA PER AVERE LA 1A DA VISUALI
118900000000     C                   Z-ADD     *ZEROS        W                 5 0
119000000000     C                   ADD       1             FUNULT
119100000000     C     FUNULT        IFGT      31
119200000000     C                   Z-ADD     1             FUNULT
119300000000     C                   ENDIF
119400000000     C* RICERCO LE OPZIONI ATTIVE E LE EMETTO A VIDEO
119500000000     C     FUNULT        DO        31            I
119600000000     C* SE HO GIA' RAGGIUNTO IL NUMERO DI FUNZIONI VISUALIZZABILI VADO A FINE
119700000000     C     W             IFLT      FUNVIS
119800000000     C* SE OPZIONE ATTIVA ED ESISTE LA DESCRIZIONE EMETTO A VIDEO
119900000000     C     FAT(I)        IFEQ      'S'
120000000000     C     FDE(I)        ANDNE     *BLANKS
120100000000     C     I             IFEQ      23
120200000000     C     I             OREQ      24
120300000000     C                   ELSE
120400000000     C                   ADD       1             W
120500000000     C                   MOVEL     FDE(I)        FUV(W)
120600000000     C                   ENDIF
120700000000     C                   ENDIF
120800000000     C* MEMORIZZO L'ULTIMO INDICE TRATTATO
120900000000     C                   Z-ADD     I             FUNULT
121000000000     C                   ENDIF
121100000000     C                   ENDDO
121200000000     C* TRASFERISCO LE OPZIONI DALLA SCHIERA AI CAMPI VIDEO
121300000000     C                   MOVEL     FUV(1)        VFD001
121400000000     C                   MOVEL     FUV(2)        VFD002
121500000000     C                   MOVEL     FUV(3)        VFD003
121600000000     C                   MOVEL     FUV(4)        VFD004
121700000000     C                   MOVEL     FUV(5)        VFD005
121800000000     C                   MOVEL     FUV(6)        VFD006
121900000000     C     FAT(23)       IFEQ      'N'
122000000000     C     FAT(24)       ANDEQ     'N'
122100000000     C                   MOVEL     FUV(7)        VFD007
122200000000     C                   MOVEL     FUV(8)        VFD008
122300000000     C                   ELSE
122400000000     C     FAT(23)       IFEQ      'S'
122500000000     C     FAT(24)       ANDEQ     'S'
122600000000     C                   MOVEL     FDE(23)       VFD007
122700000000     C                   MOVEL     FDE(24)       VFD008
122800000000     C                   ELSE
122900000000     C                   MOVEL     FUV(7)        VFD007
123000000000     C     FAT(23)       IFEQ      'S'
123100000000     C                   MOVEL     FDE(23)       VFD008
123200000000     C                   ELSE
123300000000     C                   MOVEL     FDE(24)       VFD008
123400000000     C                   ENDIF
123500000000     C                   ENDIF
123600000000     C                   ENDIF
123700000000     C                   MOVEL     '1'           $TESVD
123800000000     C                   ENDSR
123900000000     C*------------------------------------------------------------------------*
124000000000     C*  CONFUN - CONTROLLO SE LA FUNZIONE SCELTA E' ATTIVA
124100000000     C*           INPUT:  - KEY    (FUNZIONE PREMUTA - DA INFDS)
124200000000     C*           OUTPUT: - FUNOK  (OPZIONE OK S/N)
124300000000     C*                   - ERRORE SE FUNZIONE NON ATTIVA
124400000000     C*------------------------------------------------------------------------*
124500000000     C     CONFUN        BEGSR
124600000000     C* RICERCA FUNZIONE SCELTA IN TABELLA FUNZIONI
124700000000     C     1             DO        31            I
124800000000     C     KEY           IFEQ      KEYS(I)
124900000000     C* CONTROLLO SE IL TASTO E' ATTIVO
125000000000     C     FAT(I)        IFNE      'S'
125100000000     C                   MOVEL     MSG(1)        $MSG
125200000000     C                   EXSR      CARMSG
125300000000     C                   MOVEL     '1'           $ERR
125400000000     C                   ENDIF
125500000000     C                   ENDIF
125600000000     C                   ENDDO
125700000000     C                   ENDSR
125800000000     C*------------------------------------------------------------------------*
125900000000     C* CAROPZ - CARICA LE SCHIERE PER LA GESTIONE DELLE OPZIONI
126000000000     C*          INPUT:  - OPZ    (SCHIERA CARICATA AUTOMATICAMENTE DAL PGM)
126100000000     C*          OUTPUT: - OPD    (SCHIERA CON OPZIONE ALLINEATA A DESTRA)
126200000000     C*                  - OPS    (SCHIERA CON OPZIONE ALLINEATA A SINISTRA)
126300000000     C*                  - ODE    (SCHIERA DESCRIZIONI OPZIONI)
126400000000     C*                  - OAT    (SCHIERA FLAG DI OPZIONE ATTIVA S/N)
126500000000     C*------------------------------------------------------------------------*
126600000000     C     CAROPZ        BEGSR
126700000000     C                   Z-ADD     *ZEROS        OPZULT            5 0
126800000000     C     1             DO        24            I
126900000000     C                   MOVEL     OPZ(I)        OPZRIG
127000000000     C                   MOVEL     OPZOPD        OPD(I)
127100000000     C                   MOVEL     OPZOPS        OPS(I)
127200000000     C                   MOVEL     OPZDES        ODE(I)
127300020923     C     DUTLPO        IFEQ      'S'
127400000000     C     $TGES         IFEQ      'G'
127500000000     C                   MOVEL     OPZASG        OAT(I)
127600000000     C                   ELSE
127700000000     C                   MOVEL     OPZASI        OAT(I)
127800000000     C                   ENDIF
127900000000     C                   ELSE
128000000000     C     $TGES         IFEQ      'G'
128100000000     C                   MOVEL     OPZAFG        OAT(I)
128200000000     C                   ELSE
128300000000     C                   MOVEL     OPZAFI        OAT(I)
128400000000     C                   ENDIF
128500000000     C                   ENDIF
128600000000     C     OAT(I)        IFEQ      'E'
128700000000     C                   MOVEL     USER          WEDP              3
128800000000     C     WEDP          IFEQ      'EDP'
128900000000     C                   MOVEL     'S'           OAT(I)
129000000000     C                   ELSE
129100000000     C                   MOVEL     'N'           OAT(I)
129200000000     C                   ENDIF
129300000000     C                   ENDIF
129400000000     C                   ENDDO
129500000000     C                   ENDSR
129600000000     C*------------------------------------------------------------------------*
129700000000     C* OUTOPZ - EMETTE A VIDEO LE OPZIONI ABILITATE
129800000000     C*          INPUT:  - OPZULT (ULTIMA OPZIONE EMESSA NEL VIDEO PRECEDENTE)
129900000000     C*          OUTPUT: - OPZNAT (N.DI OPZIONI ATTIVE)
130000000000     C*                  - VOD001-008 (CAMPI OPZIONI A VIDEO)
130100000000     C*                  - OPZULT (ULTIMA OPZIONE EMESSA A VIDEO)
130200000000     C*------------------------------------------------------------------------*
130300000000     C     OUTOPZ        BEGSR
130400000000     C     1             DO        8             I
130500000000     C                   MOVEL     *BLANKS       OPV(I)
130600000000     C                   ENDDO
130700000000     C                   Z-ADD     *ZEROS        OPZNAT            2 0
130800000000     C* AGGIUNGO UNO ALL'ULTIMA OPZIONE VISUALIZZATA PER AVERE LA 1A DA VISUALIZ
130900000000     C                   ADD       1             OPZULT
131000000000     C     OPZULT        IFGT      24
131100000000     C                   Z-ADD     1             OPZULT
131200000000     C                   END
131300000000     C* CONTROLLO QUANTE OPZIONI ATTIVE E DA VISUALIZZARE CI SONO
131400000000     C     1             DO        24            I
131500000000     C     OAT(I)        IFEQ      'S'
131600000000     C     ODE(I)        ANDNE     *BLANKS
131700000000     C                   ADD       1             OPZNAT
131800000000     C                   ENDIF
131900000000     C                   ENDDO
132000000000     C* RICERCO LE OPZIONI ATTIVE E LE EMETTO A VIDEO
132100000000     C                   Z-ADD     0             W
132200000000     C     OPZULT        DO        24            I
132300000000     C* SE SONO GIA' STATE EMESSE 8 OPZIONI VADO A FINE CICLO
132400000000     C     W             IFLT      8
132500000000     C* SE OPZIONE ATTIVA ED ESISTE LA DESCRIZIONE EMETTO A VIDEO
132600000000     C     OAT(I)        IFEQ      'S'
132700000000     C     ODE(I)        ANDNE     *BLANKS
132800000000     C                   ADD       1             W
132900000000     C                   MOVEL     ODE(I)        OPV(W)
133000000000     C                   ENDIF
133100000000     C* MEMORIZZO L'ULTIMA DESCRIZIONE EMESSA
133200000000     C                   Z-ADD     I             OPZULT
133300000000     C                   ENDIF
133400000000     C                   ENDDO
133500000000     C* TRASFERISCO LE OPZIONI DALLA SCHIERA AI CAMPI VIDEO
133600000000     C                   MOVEL     OPV(1)        VOD001
133700000000     C                   MOVEL     OPV(2)        VOD002
133800000000     C                   MOVEL     OPV(3)        VOD003
133900000000     C                   MOVEL     OPV(4)        VOD004
134000000000     C                   MOVEL     OPV(5)        VOD005
134100000000     C                   MOVEL     OPV(6)        VOD006
134200000000     C                   MOVEL     OPV(7)        VOD007
134300000000     C                   MOVEL     OPV(8)        VOD008
134400000000     C                   MOVEL     '1'           $TESVD
134500000000     C* SE VI SONO PIU' DI 8 OPZIONI ATTIVO IL TASTO F23
134600000000     C     OPZNAT        IFGT      8
134700000000     C                   MOVEL     'S'           FAT(23)
134800000000     C                   ENDIF
134900000000     C                   ENDSR
135000000000     C*------------------------------------------------------------------------*
135100000000     C*  CONOPZ - CONTROLLO SE L'OPZIONE SCELTA E' ATTIVA
135200000000     C*           INPUT:  - VSCOPZ (OPZIONE SCELTA A VIDEO)
135300000000     C*           OUTPUT: - OPZOK  (OPZIONE OK S/N)
135400000000     C*                   - ERRORE SE OPZIONE NON ATTIVA
135500000000     C*------------------------------------------------------------------------*
135600000000     C     CONOPZ        BEGSR
135700000000     C* RICERCA OPZIONE SCELTA IN TABELLA OPZIONI
135800000000     C                   MOVEL     *BLANKS       OPZOK             1
135900000000     C     1             DO        24            I                 5 0
136000000000     C     VSCOPZ        IFEQ      OPD(I)
136100000000     C     VSCOPZ        OREQ      OPS(I)
136200000000     C     OAT(I)        IFEQ      'S'
136300000000     C                   MOVEL     OPD(I)        VSCOPZ
136400000000     C                   MOVEL     'S'           OPZOK
136500000000     C                   ELSE
136600000000     C                   MOVEL     'N'           OPZOK             1
136700000000     C                   ENDIF
136800000000     C                   ENDIF
136900000000     C                   ENDDO
137000000000     C* SE L'OPZIONE NON E' ATTIVA MANDO MESSAGGIO
137100000000     C     OPZOK         IFNE      'S'
137200000000     C                   MOVEL     MSG(3)        $MSG
137300000000     C                   EXSR      CARMSG
137400000000     C                   MOVEL     '1'           $ERR
137500000000     C                   MOVEL     'N'           OPZOK             1
137600000000     C                   ENDIF
137700000000     C                   ENDSR
137800000000** ======== SCHIERA: MSG  (MESSAGGI)                       ================
137900000000TREC79R- Tasto funzionale non valido.                                          1
138000000000TREC79R- Tasto funzionale abilitato ma non gestito. Avvertire CED.             2
138100000000TREC79R- Opzione non valida.                                                   3
138200000000TREC79R- Opzione abilitata ma non gestita. Avvertire CED.                      4
138300000000TREC79R- Inizio elenco.                                                        5
138400000000TREC79R- Fine elenco.                                                          6
138500000000TREC79R- 7
138600000000TREC79R- 8
138700000000TREC79R- 9
138800000000TREC79R- 10
138900000000TREC79R- 11
139000000000TREC79R- 12
139100000000TREC79R- 13
139200000000TREC79R- 14
139300000000TREC79R- 15
139400000000TREC79R- 16
139500000000TREC79R- 17
139600000000TREC79R- 18
139700000000TREC79R- 19
139800000000TREC79R- 20
139900000000** OPZ  (OPZIONI)         SEDE:GI FIL:GI   (G=GESTIONE,I=INTERROG.)
140000000000- 1-1 -                   -    NN     NN
140100000000- 2-2 -                   -    NN     NN
140200000000- 3-3 -                   -    NN     NN
140300000000- 4-4 -                   -    NN     NN
140400000000- 5-5 -5=Visualizzazione  -    NN     NN
140500000000- 6-6 -                   -    NN     NN
140600000000- 7-7 -                   -    NN     NN
140700000000- 8-8 -                   -    NN     NN
140800000000- 9-9 -                   -    NN     NN
140900000000-10-  -                   -    NN     NN
141000000000-11-  -                   -    NN     NN
141100000000-12-  -                   -    NN     NN
141200000000-13-  -                   -    NN     NN
141300000000-14-  -                   -    NN     NN
141400000000-15-  -                   -    NN     NN
141500000000-16-  -                   -    NN     NN
141600000000-17-  -                   -    NN     NN
141700000000-18-  -                   -    NN     NN
141800000000-19-  -                   -    NN     NN
141900000000-20-  -                   -    NN     NN
142000000000-21-  -                   -    NN     NN
142100000000-22-  -                   -    NN     NN
142200000000-23-  -                   -    NN     NN
142300000000-24-  -                   -    NN     NN
142400000000** FUN2 (SUBFILE)         SEDE:GI FIL:GI   (G=GESTIONE,I=INTERROG.)
142500000000F1    -                   -    NN     NN
142600000000F2    -                   -    NN     NN
142700000000F3    -F3=Fine            -    SS     SS
142800000000F4    -                   -    NN     NN
142900000000F5    -                   -    NN     NN
143000000000F6    -                   -    NN     NN
143100000000F7    -                   -    NN     NN
143200000000F8    -                   -    NN     NN
143300000000F9    -                   -    NN     NN
143400000000F10   -                   -    NN     NN
143500000000F11   -                   -    NN     NN
143600000000F12   -F12=Ritorno        -    SS     SS
143700000000F13   -                   -    NN     NN
143800000000F14   -                   -    NN     NN
143900000000F15   -                   -    NN     NN
144000000000F16   -                   -    NN     NN
144100000000F17   -F17=Inizio dati    -    SS     SS
144200000000F18   -F18=Fine dati      -    SS     SS
144300000000F19   -                   -    NN     NN
144400000000F20   -                   -    NN     NN
144500000000F21   -                   -    NN     NN
144600000000F22   -                   -    NN     NN
144700000000F23   -F23=Altre opzioni  -    NN     NN
144800000000F24   -F24=Altri tasti    -    NN     NN
144900000000ENTER -                   -    SS     SS
145000000000ROLLUP-                   -    SS     SS
145100000000ROLLDN-                   -    SS     SS
145200000000HELP  -                   -    NN     NN
145300000000PRINT -                   -    NN     NN
145400000000BSPACE-                   -    NN     NN
145500000000CLEAR -                   -    NN     NN
