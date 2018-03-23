000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200010509     H*-------------------------------------------------------------------------*
000300020613     H* STATISTICA EVENTI CLIENTE WIND
000400010509     H*-------------------------------------------------------------------------*
000500020614     FTITAS31C  IF   E           K DISK
000600010131     F                                     rename(titas000:titasx00)
000700010131     F                                     rename(titas010:titasx10)
000800010131     F                                     rename(titasP00:titasxP0)
000900030603     FFNLBL01L  IF   E           K DISK
001000030414     FFNEVB05L  IF   E           K DISK
001100030414     FTABEL00F  IF   E           K DISK
001200030414     FTIST57P   O    E             PRINTER OFLIND(*IN01)
001300001218     D*--------------------
001400001218     D* DS ESTERNE
001500001218     D*--------------------
001600900517     D KPJBA         E DS
001700040312     D*---
001800040312     D* DS TIBI BOLLA
001900040312     D*---
002000040312     D DSTB          E DS
002100040312     D*---
002200040312     D* DS FLAG TIPI BOLLA
002300040312     D*---
002400040312     D DS2W          E DS
002500011112     D*--------------------
002600011112     D* DS DI RIDEFINIZIOINE CAMPO DATA SPEDIZIONE TAS
002700011112     D*--------------------
002800011112     D                 DS
002900011112     D  TASAAS                        4  0
003000011112     D  TASMGS                        4  0
003100011112     D  TASDSP                 1      8  0
003200020613     D*------------------
003300020613     D* DS X PASSAGGIO PARAMETRI
003400020613     D*------------------
003500020613     D PARAM           DS                  INZ
003600020613     D  PARKSC                 1      7  0
003700020613     D  PARDAI                 8     15  0
003800020613     D  PARDAF                16     23  0
003900030414     D  PARDAI2               24     31  0
004000030414     D  PARDAF2               32     39  0
004100030414     D  PAROTQ                40     49
004200030414     D  PARRAG                50     89
004300030414     D*--------------------
004400030414     D* VARIABILI DI WRK
004500030414     D*--------------------
004600030414     D depCEV          s                   like(evbCEV)
004700030414     D wPriorita       s              1  0 inz(*zeros)
004800030414     D wDepPriorita    s              1  0 inz(*zeros)
004900030414     D ds2a          e ds
005000030414     D sk2Ainderr      s              3    dim(5) ctdata perrcd(1)
005100030414     D sk2Arif         s              3    dim(20) ctdata perrcd(1)
005200080903     D sk2Agiac        s              3    dim(200)
005300030414     D*---
005400030414     D* DS REPERIMENTO DATI UTENTE
005500030414     D*---
005600030414     D TIBS34DS      E DS                                                       *Profili utente
005700030414     D DDATIUTE      E DS                                                       *Dati utente
005800030414     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
005900040312     D*---
006000040312     D* TIPI BOLLA
006100040312     D*---
006200040312     D TBL             S              2    DIM(20)                              *TIPO BOLLA
006300040312     D TRBL            S              1    DIM(20)                              *RAGGRUPPAMENTO
006400040312     D TFSP            S              1    DIM(20)                              *FLAG SPEDIZIONI (S/
006500040312     D TFRI            S              1    DIM(20)                              *FLAG RICAVI     (S/
006600001218
006700001218
006800001218
006900920812     C*---------------------------------------------------------------*
007000001218     C* MAIN
007100001218     C*---------------------------------------------------------------*
007200030414     C                   exsr      cartab
007300001218     C                   exsr      procedi
007400030414     C                   exsr      stampa
007500001218     C*
007600001218     C                   seton                                        LR
007700010509     C*---------------------------------------------------------------*
007800001218
007900001218
008000001218
008100001218     C*------------------------------------------------------------------------*
008200001218     C* PROCEDI - Routine principale
008300001218     C*------------------------------------------------------------------------*
008400020613     C     PROCEDI       BEGSR
008500001218     C*
008600020614     C* Mi posiziono sul TITAS31C per cliente fatturazione
008700020613     C                   Z-ADD     PARKSC        tasKSC
008800030414     C     KEYtas31cP    SETLL     TITAS31C
008900020613     C*
009000020613     C* Se posizionamento riuscito => ciclo ed elaboro
009100020614     C                   IF        %found(TITAS31C)
009200020614     C                   MOVEL     '0'           flgFINE           1
009300020613     C*
009400020613     C* Prima lettura fuori ciclo
009500020613     C                   EXSR      LEGGITAS
009600020614     C                   DOW       not %eof(TITAS31C) AND
009700020614     C                             flgFINE = '0'
009800020613     C*
009900020613     C* Verifico se la bolla è da considerare
010000020613     C                   EXSR      CHKRECTAS
010100020613     C                   IF        flgRECOK = 'S'
010200020613     C*
010300030414     C* Se la bolla è da considerare => totalizzo spedizione
010400030414     C                   EXSR      MEMSPE
010500020613     C                   ENDIF
010600020613     C*
010700020613     C* Proseguo le letture dentro ciclo
010800020613     C                   EXSR      LEGGITAS
010900020613     C                   ENDDO
011000001218     C*
011100020613     C                   ENDIF
011200020613     C*
011300001218     C                   ENDSR
011400001218     C*------------------------------------------------------------------------*
011500020613
011600020613
011700020613
011800020613     C*------------------------------------------------------------------------*
011900030414     C* LEGGITAS - Routine di lettura record TITAS31C
012000020613     C*------------------------------------------------------------------------*
012100020613     C     LEGGITAS      BEGSR
012200020613     C*
012300020614     C                   READ      TITAS31C
012400020613     C*
012500020613     C                   ENDSR
012600020613     C*------------------------------------------------------------------------*
012700010627
012800010627
012900010627
013000010627     C*------------------------------------------------------------------------*
013100020613     C* CHKRECTAS - Routine di verifica bolla
013200010627     C*------------------------------------------------------------------------*
013300020613     C     CHKRECTAS     BEGSR
013400010627     C*
013500020613     C                   MOVEL     'S'           flgRECOK          1
013600020613     C*
013700020613     C* Testo se la bolla è del cliente richiesto
013800030603     C                   IF        flgRECOK = 'S'
013900020613     C                   IF        tasKSC = PARKSC
014000020613     C                   ELSE
014100020613     C                   MOVEL     'N'           flgRECOK
014200020614     C* quando cambia il cliente esco dal ciclo
014300020614     C                   MOVEL     '1'           flgFINE
014400020613     C                   ENDIF
014500030603     C                   ENDIF
014600020613     C*
014700020613     C* Testo se la bolla è del periodo richiesto
014800030603     C                   IF        flgRECOK = 'S'
014900020613     C                   IF        tasDSP >= PARDAI AND
015000020613     C                             tasDSP <= PARDAF
015100020613     C                   ELSE
015200020613     C                   MOVEL     'N'           flgRECOK
015300020613     C                   ENDIF
015400030603     C                   ENDIF
015500040312     C*
015600040312     C* Testo se il tipo bolla è d quelli validi ai fini della spedizione
015700040312     C                   IF        flgRECOK = 'S'
015800040312     C                   SETOFF                                           28
015900040312     C                   Z-ADD     1             I
016000040312     C     TASTBL        LOOKUP    TBL(I)                                 28
016100040312     C     *IN28         IFEQ      *OFF                                         *NON TROVATO
016200040312     C     TFSP(I)       ORNE      'S'
016300040312     C                   MOVEL     'N'           flgRECOK
016400040312     C                   ENDIF
016500040312     C                   ENDIF
016600030603     C*
016700030603     C* Testo che la bolla nn sia 1 bolla "figlia"
016800030603     C                   IF        flgRECOK = 'S'
016900030603     C     KEYlbl01C     CHAIN     FNLBL01L
017000030603     C                   IF        %found(FNLBL01L)
017100030603     C                   MOVEL     'N'           flgRECOK
017200030603     C                   ENDIF
017300030603     C                   ENDIF
017400010627     C*
017500010627     C                   ENDSR
017600010627     C*------------------------------------------------------------------------*
017700010626
017800010626
017900010626
018000010626
018100010509     C*------------------------------------------------------------------------*
018200030414     C* MEMSPE - Routine di totalizzazione spedizioni
018300010509     C*------------------------------------------------------------------------*
018400030414     C     MEMSPE        BEGSR
018500010509     C*
018600030414     C*
018700030414     C* Innanzitutto elaboro solo le bolle "chiuse" con reso e aventi una apertura giacenza
018800030414 IF 1C                   IF        tasDCM > *zeros AND
018900030429     C                             tasCCA = '2'
019000030521     C*
019100030521     C* Elaboro solo le bolle di reso aventi numero distinta consegna <> 9999999
019200030711 IF 2C                   IF        tasNDC <> 9999999
019300030414     C*
019400030414     C* ...quindi verifico il tipo di evento da assegnare alla spedizione
019500030414     C* (tengo l'ultimo evento di tipo giacenza)
019600030414     C                   CLEAR                   depCEV
019700030414     C                   CLEAR                   wPriorita
019800030414     C                   CLEAR                   wDepPriorita
019900030414     C     KEYevb05P     SETLL     fnevb05l
020000030414     C     KEYevb05P     READE     fnevb05l
020100030414     C                   DOW       not %eof(fnevb05l)
020200030414     C     evbCEV        LOOKUP    sk2Agiac                               19
020300030414     C                   IF        *IN19 = *ON
020400030414     C                   IF        evbCEV = 'G'
020500030414     C                   EVAL      wPriorita = 0
020600030414     C                   ELSE
020700030414     C                   EVAL      wPriorita = 1
020800030414     C                   ENDIF
020900030414     C* Considero l'evento 'G  ' secondario rispetto a tutti gli altri
021000030414     C                   IF        wPriorita >= wDepPriorita
021100030414     C                   EVAL      wDepPriorita = wPriorita
021200030414     C                   EVAL      depCEV = evbCEV
021300030414     C                   ENDIF
021400030414     C                   ENDIF
021500030414     C     KEYevb05P     READE     fnevb05l
021600030414     C                   ENDDO
021700030414     C*
021800030414     C* ...distinguo quindi l'evento x identificare quale file scrivere
021900030711 IF 3C                   IF        depCEV <> *blanks
022000030414     C     depCEV        LOOKUP    sk2Ainderr                             20
022100030711 IF 4C                   IF        *IN20 = *ON
022200030414     C* => Indirizzi errati
022300030414     C                   EVAL      STPQNCIE = STPQNCIE + 1
022400030414     C                   EVAL      STPTOTSPE = STPTOTSPE + 1
022500030711 X  4C                   ELSE
022600030414     C     depCEV        LOOKUP    sk2Arif                                21
022700030711 IF 5C                   IF        *IN21 = *ON
022800030414     C* => Rifiuti destinatario
022900030414     C                   EVAL      STPQNCR = STPQNCR + 1
023000030414     C                   EVAL      STPTOTSPE = STPTOTSPE + 1
023100030711 X  5C                   ELSE
023200030414     C* => Altri motivi di reso
023300030414     C                   EVAL      STPQNCA = STPQNCA + 1
023400030414     C                   EVAL      STPTOTSPE = STPTOTSPE + 1
023500030711 E  5C                   ENDIF
023600030711 E  4C                   ENDIF
023700030711 X  3C                   ELSE
023800030711     C* => Altri motivi di reso
023900030711     C                   EVAL      STPQNCA = STPQNCA + 1
024000030711     C                   EVAL      STPTOTSPE = STPTOTSPE + 1
024100030711 E  3C                   ENDIF
024200030711     C                   ELSE
024300030711 X  2C* Se trattasi di resi "9999999" li conteggio come W.I.P. (Work In Progress)
024400030603     C                   EVAL      STPQIP = STPQIP + 1
024500030603     C                   EVAL      STPTOTSPE = STPTOTSPE + 1
024600030711 E  2C                   ENDIF
024700030414     C*
024800030414 X  1C                   ELSE
024900030414     C*
025000030414     C* X le spedizioni OK...
025100030414 IF 2C                   IF        tasDCM > *zeros  AND
025200030414     C                             tasCCA = *blanks
025300030414     C                   EVAL      STPQCO = STPQCO + 1
025400030414     C                   EVAL      STPTOTSPE = STPTOTSPE + 1
025500030414     C*
025600030414 END2C                   ENDIF
025700030414 END1C                   ENDIF
025800030414     C*
025900030414     C* Per le spedizioni nn ancora chiuse
026000030414 IF 3C                   IF        tasDCM = *zeros
026100030414     C                   EVAL      STPQIP = STPQIP + 1
026200030414     C                   EVAL      STPTOTSPE = STPTOTSPE + 1
026300030414 END3C                   ENDIF
026400010509     C*
026500010509     C                   ENDSR
026600010509     C*---------------------------------------------------------------*
026700030414
026800030414
026900030414
027000030414
027100030414     C*------------------------------------------------------------------------*
027200030603     C* STAMPA - Routine di calcolo percentuali e stampa prospetto riepilogativo
027300030414     C*------------------------------------------------------------------------*
027400030414     C     STAMPA        BEGSR
027500030414     C*
027600030414     C* Solo se totalizzato qualcosa
027700030414     C                   IF        STPTOTSPE > *zeros
027800030414     C*
027900030414     C* % IN PROCESS
028000030414     C                   EVAL      STPPIP = STPQIP * 100 / STPTOTSPE
028100030414     C*
028200030414     C* % CONSEGNATE
028300030414     C                   EVAL      STPPCO = STPQCO * 100 / STPTOTSPE
028400030414     C*
028500030414     C* % NON CONSEGNATE - INDIRIZZI ERRATI
028600030414     C                   EVAL      STPPNCIE = STPQNCIE * 100 / STPTOTSPE
028700030414     C*                  - RIFIUTI
028800030414     C                   EVAL      STPPNCR = STPQNCR * 100 / STPTOTSPE
028900030414     C*                  - ALTRI MOTIVI
029000030414     C                   EVAL      STPPNCA = STPQNCA * 100 / STPTOTSPE
029100030414     C*
029200030414     C                   ENDIF
029300030414     C*
029400030414     C* IMPOSTO PARAMETRI DI LANCIO X TESTATA
029500030414     C                   EVAL      STPSYF = RSUT
029600030414     C                   EVAL      STPKSC = PARKSC
029700030414     C                   EVAL      STPRAG = PARRAG
029800030414     C                   EVAL      STPDAI = PARDAI2
029900030414     C                   EVAL      STPDAF = PARDAF2
030000030414     C*
030100030414     C* STAMPO TESTATA
030200030414     C                   WRITE     ST56PT
030300030414     C                   WRITE     ST56T1
030400030414     C                   WRITE     ST56PI
030500030414     C                   WRITE     ST56P2
030600030414     C                   WRITE     ST56RV
030700030414     C                   WRITE     ST56TT
030800030414     C                   WRITE     ST56RI
030900030414     C                   WRITE     ST56FI
031000030414     C*
031100030414     C                   ENDSR
031200030414     C*------------------------------------------------------------------------*
031300030414
031400030414
031500030414
031600030414     C*---------------------------------------------------------------*
031700030414     C* CARTAB - Caricamento tabelle di wrk
031800030414     C*---------------------------------------------------------------*
031900030414     C     CARTAB        BEGSR
032000030414     C*
032100040312     C*---
032200040312     C* CAUSALI EVENTI
032300040312     C*---
032400030414     C                   clear                   sk2Agiac
032500030414     C                   z-add     *zeros        i                 3 0
032600030414     C                   eval      tblKUT = 1
032700030414     C                   eval      tblCOD = '2A'
032800030414     C     KEYtabP       setll     tabel00f
032900030414     C     KEYtabP       reade     tabel00f
033000030414     C                   dow       not %eof(tabel00f)
033100030414     C                   movel(p)  tblUNI        ds2a
033200030414     C                   if        §2AFTC = 'G'
033300030414     C                   add       1             i
033400030414     C                   eval      sk2Agiac(i) = tblKEY
033500030414     C                   endif
033600030414     C     KEYtabP       reade     tabel00f
033700030414     C                   enddo
033800040312     C*---
033900040312     C* TIPI BOLLA
034000040312     C*---
034100040312     C                   Z-ADD     *ZEROS        I
034200040312     C                   Z-ADD     1             tblKUT
034300040312     C                   MOVEL     'TB'          tblCOD
034400040312     C     KEYtabP       SETLL     TABEL00F
034500040312     C     KEYtabP       READE     TABEL00F                               99
034600040312     C     *IN99         DOWEQ     *OFF
034700040312     C     TBLFLG        IFEQ      *BLANKS
034800040312     C                   MOVEL     TBLUNI        DSTB
034900040312     C                   ADD       1             I
035000040312     C                   MOVEL     TBLKEY        TBL(I)
035100040312     C                   MOVEL     §TBRBL        TRBL(I)
035200040312     C                   ENDIF
035300040312     C     KEYtabP       READE     TABEL00F                               99
035400040312     C                   ENDDO
035500040312     C*---
035600040312     C* FLAG TIPI BOLLA
035700040312     C*---
035800040312     C     1             DO        20            I
035900040312     C     TBL(I)        IFNE      *BLANKS
036000040312     C                   Z-ADD     1             tblKUT
036100040312     C                   MOVEL     '2W'          tblCOD
036200040312     C                   MOVEL(P)  TRBL(I)       tblKEY
036300040312     C     KEYtab        CHAIN     TABEL00F                           99
036400040312     C     *IN99         IFEQ      *OFF
036500040312     C                   MOVEL     TBLUNI        DS2W
036600040312     C                   MOVEL     §2WFSP        TFSP(I)                        *FLAG SPEDIZIONI
036700040312     C                   MOVEL     §2WFRI        TFRI(I)                        *FLAG RICAVI
036800040312     C                   ENDIF
036900040312     C                   ENDIF
037000040312     C                   ENDDO                                                  *FINE TIPI BOLLA
037100030414     C*
037200030414     C                   ENDSR
037300030414     C*------------------------------------------------------------------------*
037400030414
037500030414
037600030414
037700030414
037800030414     C*------------------------------------------------------------------------*
037900030414     C* REPERISCE I DATI UTENTE
038000030414     C*------------------------------------------------------------------------*
038100030414     C     REPDATIUTE    BEGSR
038200030414     C*
038300030414     C* INIZIALIZZA VARIABILI DI WRK
038400030414     C                   CLEAR                   TIBS34DS
038500030414     C                   CLEAR                   AZUTEDS
038600030414     C                   CLEAR                   DDATIUTE
038700030414     C*
038800030414     C     *DTAARA       DEFINE    §azute        azuteds
038900030414     C     *DTAARA       DEFINE    §datiute      ddatiute
039000030414     C                   IN(E)     *DTAARA
039100030414if  1C                   IF        %Error
039200030414     c                   EVAL      I34Tla = 'L'
039300030414     C                   CALL      'TIBS34R'
039400030414     C                   PARM                    Tibs34Ds
039500030414     C                   IN        *DTAARA
039600030414e   1C                   ENDIF
039700030414     C*
039800030414     C                   ENDSR
039900030414     C*------------------------------------------------------------------------*
040000010509
040100010509
040200001218
040300001218     C*------------------------------------------------------------------------*
040400001218     C* *INZSR - ROUTINE INIZIALE
040500001218     C*------------------------------------------------------------------------*
040600001218     C     *INZSR        BEGSR
040700001218     C*
040800010726     C* Ricevo in input il parametro indicante l'ID LANCIO
040900010726     C     *ENTRY        PLIST
041000020613     C                   PARM                    KPJBA
041100020613     C*
041200020613     C                   MOVEL     KPJBU         PARAM
041300010726     C*
041400030414     C                   Z-ADD     1             tblKUT
041500030414     C****
041600030414     C* Definizione chiavi
041700030414     C****
041800030414     C* Chiave su TABEL00F - parziale
041900030414     C     KEYtabP       KLIST
042000030414     C                   KFLD                    tblKUT
042100030414     C                   KFLD                    tblCOD
042200040312     C* Chiave su TABEL00F - completa
042300040312     C     KEYtab        KLIST
042400040312     C                   KFLD                    tblKUT
042500040312     C                   KFLD                    tblCOD
042600040312     C                   KFLD                    tblKEY
042700030414     C* Chiave su TITAS31C - parziale
042800030414     C     KEYtas31cP    KLIST
042900030414     C                   KFLD                    tasKSC
043000030414     C* Chiave su FNEVB05L - parziale
043100030414     C     KEYevb05P     KLIST
043200030414     C                   KFLD                    tasAAS
043300030414     C                   KFLD                    tasLNP
043400030414     C                   KFLD                    tasNRS
043500030414     C                   KFLD                    tasNSP
043600030603     C* Chiave su FNLBL01L - completa
043700030603     C     KEYlbl01C     KLIST
043800030603     C                   KFLD                    tasAAS
043900030603     C                   KFLD                    tasLNP
044000030603     C                   KFLD                    tasNRS
044100030603     C                   KFLD                    tasNSP
044200030414     C*
044300030414     C* REPERIMEMTO DATI UTENTE
044400030414     C                   EXSR      REPDATIUTE
044500001218     C*
044600001218     C                   ENDSR
044700030414** sk2Ainderr - SCHIERA CODICI EVENTO X INDIRIZZO ERRATO
044800030414019
044900030414020
045000030414021
045100030414022
045200030414
045300030414** sk2Arif    - SCHIERA CODICI EVENTO X RIFIUTO DESTINATARIO
045400030414001
045500030414002
045600030414003
045700030414004
045800030414005
045900030414007
046000030414008
046100030414010
046200030414012
046300030414013
046400030414014
046500030414027
046600030414029
046700030414030
046800030414031
046900030414RFM
047000030414RFD
047100030414
047200030414
047300030414
