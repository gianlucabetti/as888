000100970526     H*------------------------------------------------------------------------*
000200001020     H* ESTENSIONE ANAGRAFICA CLIENTI ABILITATI
000300970526     H*------------------------------------------------------------------------*
000400970526     H DECEDIT('0,') DATEDIT(*DMY.)
000500970526     F*------------------------------------------------------------------------*
000600970526     F* DATA BASE
000700970526     F*------------------------------------------------------------------------*
000800001020     Ftis733d   cf   e             WORKSTN INFDS(DEVDS1)
000900001020     F                                     SFILE(s733DMS:MSGULT)
001000970527     F*------------
001100001020     Ftiabl01L  UF A E           K DISK
001200001020     Ftianv01L  IF   E           K DISK
001300090622     Fsgius     IF   E           K DISK
001400060607     Fazlin03l  IF   E           K DISK
001500970526     D*------------------------------------------------------------------------*
001600970526     D* SCHIERE
001700970526     D*------------------------------------------------------------------------*
001800970526     D*------------
001900970526     D* DECODIFICHE
002000970526     D*------------
002100991014     D DEC             S             66    DIM(6) CTDATA PERRCD(1)
002200970526     D*------------
002300970526     D* MESSAGGI
002400970526     D*------------
002500970527     D MSG             S             78    DIM(20) CTDATA PERRCD(1)
002600970526     D*------------
002700970526     D* DESCRIZIONI TASTI FUNZIONALI
002800970526     D*------------
002900970526     D FUD             S             80    DIM(24) CTDATA PERRCD(1)
003000970526     D*------------
003100970526     D* FUNZIONI DA CARICARE A VIDEO
003200970526     D*------------
003300970526     D FUV             S             19    DIM(24)
003400970526     D*------------------------------------------------------------------------*
003500970526     D* INPUT
003600970526     D*------------------------------------------------------------------------*
003700970526     D*------------
003800970526     D* RIDEFINIZIONE FUNZIONI VIDEO
003900970526     D*------------
004000970526     D FUDDS           DS
004100970526     D  FUDDE1                 1      1
004200970526     D  FUDFUN                 2      3
004300970526     D  FUDDE2                 4     19
004400970526     D  FUDFI0                20     34
004500970526     D  FUDATT                35     35
004600970526     D  FUDFI1                36     80
004700970526     D  FUDDES                 1     19
004800970526     D  FUDRIG                 1     80
004900970526     D*------------
005000970526     D* DS GESTIONE TASTI FUNZIONALI
005100970526     D*------------
005200970526     D DSKEY         E DS
005300970526     D DEVDS1          DS
005400970526     D  KEY                  369    369
005500970526     D*------------
005600970526     D* DS "XMSG" - GESTIONE MESSAGGI
005700970526     D*------------
005800970526     D DSMSG         E DS
005900001020     D*------------
006000001020     D* DS - REPERIMENTO TABELLE
006100001020     D*------------
006200991108     D TIBS02DS      E DS
006300970528     D*------------------
006400970526     D* DS DI PROCEDURA
006500970526     D*------------
006600001020     D tis732        E DS                  EXTNAME(tis732ds)
006700060607     D ablFl1        E DS
006800060607     D                                     QUALIFIED
006900970526     D*------------
007000001020     D* DS DI REPERIMENTO DATI CLIENTE
007100001020     D*------------
007200001020     DBS69DS         e ds                  extname(tibs69ds)
007300001020     DACODS          e ds                  extname(cnaco00f)
007400001020     DINDDS          e ds                  extname(cnind00f)
007500001020     DCLPDS          e ds                  extname(cnclp00f)
007600001020     DCLSDS          e ds                  extname(fncls00f)
007700001120     D*------------------
007800001120     D* DS X CONTROLLO INDIRIZZO E-MAIL
007900001120     D*------------
008000001120     D DSEMAIL       E DS
008100001020     D*------------
008200970526     D* ARCHITETTURA
008300970526     D*------------
008400970526     D KPJBA         E DS
008500010313     D*------------------
008600010313     D* VARIABILI DI WRK
008700010313     D*------------------
008800010313     D API             s              1    inz('''')
008900100503     D
009000100503     D                 ds
009100100615     D  val                          63A   INZ(' ABCDEFGHIJKLMNOPQRSTUVWXYZ0123-
009200100615     D                                     456789abcdefghijklmnopqrstuvwxyz')
009300100615     D  valAry                        1A   DIM(63)
009400100503     D                                     OVERLAY(val)
009500970526     C*------------------------------------------------------------------------*
009600970526     C* MAIN LINES
009700970526     C*------------------------------------------------------------------------*
009800970526     C*
009900970526     C* NON CI SONO ERRORI IN ENTRATA
010000001020     C     ERRL00        IFEQ      '0'
010100970526     C*
010200970526     C* CICLO FINO A CHE NON VIENE PREMUTO F03 O F12
010300001020     C     F03L00        DOUEQ     '1'                                          *F03 PREMUTO
010400970526     C     F12L00        OREQ      '1'                                          *F12 PREMUTO
010500970526     C     FINL00        OREQ      '1'                                          *FINE PGM
010600970526     C*
010700970526     C* EMETTE FORMATO VIDEO
010800970526     C                   EXSR      BUTFMR
010900970526     C*
011000970526     C* GESTIONE TASTI FUNZIONALI
011100970526     C                   EXSR      FUNCON
011200001020     C                   ENDDO
011300970526     C*
011400970526     C* CI SONO ERRORI IN ENTRATA
011500001020     C                   ELSE
011600001020     C                   MOVEL     '1'           s32err
011700001020     C                   MOVEL     DSMSMS        s32msg
011800001020     C                   ENDIF
011900970526     C* FINE PGM
012000970526     C                   SETON                                        LR
012100970526     C*------------------------------------------------------------------------*
012200970526     C* BUTFMR - EMISSIONE FORMATO A VIDEO
012300970526     C*------------------------------------------------------------------------*
012400970526     C     BUTFMR        BEGSR
012500970526     C*
012600970526     C* EMETTE LA TESTATA
012700001020     C                   WRITE     s733D01
012800970526     C*
012900970526     C* SPEDISCE EVENTUALE MESSAGGIO
013000970526     C                   EXSR      SNDMSG
013100970526     C*
013200970526     C* EMETTE LE FUNZIONI
013300001020     C                   WRITE     s733D02
013400970526     C*
013500970526     C* EMETTE IL VIDEO
013600001020     C                   EXFMT     s733D03
013700970526     C*
013800970526     C* REIMPOSTA FLAG DI ERRORE
013900970526     C                   MOVEL     '0'           ERRL00
014000970526     C*
014100970526     C* REIMPOSTA INDICATORI DI ERRORE
014200001024     C                   SETOFF                                       606162
014300001024     C                   SETOFF                                       636465
014400010227     C                   SETOFF                                       666780
014500060607     C                   SETOFF                                       68
014600970526     C*
014700970526     C                   ENDSR
014800970526     C*------------------------------------------------------------------------*
014900970526     C* SNDMSG - SPEDISCE EVENTUALE MESSAGGIO
015000970526     C*------------------------------------------------------------------------*
015100970526     C     SNDMSG        BEGSR
015200970526     C*
015300970526     C* PULISCE IL SFILE MESSAGGI
015400970526     C                   MOVEL     '1'           *IN40
015500001020     C                   WRITE     s733DMC
015600970526     C                   MOVEL     '0'           *IN40
015700970526     C*
015800970526     C* VISUALIZZA IL MESSAGGIO
015900970526     C                   Z-ADD     *ZEROS        MSGULT            4 0
016000970526     C                   CALL      'XMSG'        XMSGPA
016100001020     C     DSMSER        IFEQ      *ZEROS
016200970526     C                   MOVEL     DSMSKE        MSGKEY
016300001020     C                   ENDIF
016400970526     C                   Z-ADD     1             MSGULT
016500001020     C                   WRITE     s733DMS
016600001020     C                   WRITE     s733DMC
016700970526     C*
016800970526     C* MANDA MESSAGGIO A *BLANKS PER PULIRLO
016900970526     C                   MOVEL     *BLANKS       DSMSMS
017000970526     C*
017100970526     C                   ENDSR
017200970526     C*------------------------------------------------------------------------*
017300970526     C* ENTGES - GESTIONE ENTER -> INVIO
017400970526     C*------------------------------------------------------------------------*
017500970526     C     ENTGES        BEGSR
017600970526     C*
017700970526     C* CONTROLLA I CAMPI IMMESSI
017800970526     C                   EXSR      DECODI
017900970526     C*
018000970526     C* SE DA VISUALIZZAZIONE CON ENTER RITORNA AL PGM CHIAMANTE
018100001020     C     DEPOP1        IFEQ      'O05'
018200970526     C                   MOVEL     '1'           FINL00
018300001020     C                   ENDIF
018400970526     C*
018500970526     C                   ENDSR
018600970526     C*------------------------------------------------------------------------*
018700970526     C* F03GES - GESTIONE F03 -> FINE
018800970526     C*------------------------------------------------------------------------*
018900970526     C     F03GES        BEGSR
019000970526     C*
019100970526     C* IMPOSTA "PREMUTO F03"
019200970526     C                   MOVEL     '1'           F03L00                         *A PROGRAMMA
019300001020     C                   MOVEL     '1'           s32F03                         *DI PROCEDURA
019400970526     C*
019500970526     C                   ENDSR
019600970526     C*------------------------------------------------------------------------*
019700970526     C* F06GES - GESTIONE F06 -> AGGIORNAMENTO
019800970526     C*------------------------------------------------------------------------*
019900970526     C     F06GES        BEGSR
020000970526     C*
020100970527     C* LANCIA LA ROUTINE DI CONFERMA INTERESSATA
020200001020     C     DEPOP1        IFEQ      'F06'
020300970526     C                   EXSR      INSERI
020400001020     C                   ENDIF
020500001020     C     DEPOP1        IFEQ      'O02'
020600970526     C                   EXSR      MODIFI
020700001020     C                   ENDIF
020800010810     C     DEPOP1        IFEQ      'O08'
020900010810     C                   EXSR      MODIFIEDP
021000010810     C                   ENDIF
021100970526     C*
021200970526     C                   ENDSR
021300970526     C*------------------------------------------------------------------------*
021400970526     C* F12GES - GESTIONE F12 -> RITORNO
021500970526     C*------------------------------------------------------------------------*
021600970526     C     F12GES        BEGSR
021700970526     C*
021800970526     C* IMPOSTA "PREMUTO F12"
021900970526     C                   MOVEL     '1'           F12L00                         *A PROGRAMMA
022000001020     C                   MOVEL     '1'           s32F12                         *DI PROCEDURA
022100970526     C*
022200970526     C                   ENDSR
022300970526     C*------------------------------------------------------------------------*
022400970526     C* F24GES - GESTIONE F24 -> ALTRE FUNZIONI
022500970526     C*------------------------------------------------------------------------*
022600970526     C     F24GES        BEGSR
022700970526     C*
022800970526     C                   EXSR      FUNGES                                       *GESTIONE FUNZIONI
022900970526     C*
023000970526     C                   ENDSR
023100970526     C*------------------------------------------------------------------------*
023200970526     C* INSERI - OPERAZIONI DI INSERIMENTO RECORD
023300970526     C*------------------------------------------------------------------------*
023400970526     C     INSERI        BEGSR
023500970526     C*
023600970526     C* CONTROLLA I CAMPI IMMESSI
023700970526     C                   EXSR      DECODI
023800970526     C*
023900970526     C* IMPOSTA I CAMPI DEL RECORD DA SCRIVERE
024000001020     C     ERRL00        IFEQ      '0'                                          *NO ERRORI
024100001020     c                   EXSR      IMPabl
024200001020     C                   WRITE     tiabl000
024300001020     C                   MOVEL     MSG(2)        s32MSG                         *MESSAGGIO
024400970602     C*
024500970602     C* PULISCE LA VIDEATA PER UN ALTRO INSERIMENTO
024600970602     C                   EXSR      AZZVID
024700170111     C*
024800170111     C* ASSEGNA I PARAMETRI IN INPUT A VIDEO (SE PASSATI)
024900170111     C                   EXSR      PASSPAR
025000001020     C                   ENDIF
025100970526     C*
025200970526     C                   ENDSR
025300970526     C*------------------------------------------------------------------------*
025400001116     C* MODIFI - OPERAZIONI DI MODIFICA
025500970526     C*------------------------------------------------------------------------*
025600970526     C     MODIFI        BEGSR
025700970526     C*
025800970526     C* CONTROLLA I CAMPI IMMESSI
025900970526     C                   EXSR      DECODI
026000970526     C*
026100970526     C* LEGGE IL RECORD PERL'AGGIORNAMENTO
026200001020     C     ERRL00        IFEQ      '0'                                          *NO ERRORI
026300001020     C                   EXSR      LETabl
026400001116     C*
026500100907     C                   MOVEL     VIDSGIANAM    alfaute
026600001116     C                   EVAL      alfaeml = VIDem1 + VIDem2
026700010726     C* EFFETTUO CONTROLLI ED ESEGUO SERIE COMANDI REMOTI SU RAGIONE SOCIALE E RESPONSABILE
026800010726     C                   Z-ADD     *zeros        pos
026900010726     C                   EVAL      pos = %scan(api:VDDksu)
027000010726     C                   DOW       pos > *zeros
027100010726     C                   EVAL      VDDksu = %replace(' ':VDDksu:pos)
027200010726     C                   Z-ADD     *zeros        pos
027300010726     C                   EVAL      pos = %scan(api:VDDksu)
027400010726     C                   ENDDO
027500010726     C*
027600010726     C                   Z-ADD     *zeros        pos
027700010726     C                   EVAL      pos = %scan(api:VIDrsp)
027800010726     C                   DOW       pos > *zeros
027900010726     C                   EVAL      VIDrsp = %replace(' ':VIDrsp:pos)
028000010726     C                   Z-ADD     *zeros        pos
028100010726     C                   EVAL      pos = %scan(api:VIDrsp)
028200010726     C                   ENDDO
028300010726     C*
028400010810     C*                  CALL(e)   'TIS730R1'
028500100906     C*                  PARM                    alfaute          40
028600010810     C*                  PARM      VIDpwd        alfapwd          20
028700010810     C*                  PARM      VDDksu        alfarag          20
028800010810     C*                  PARM      VIDrsp        alfarsp          30
028900010810     C*                  PARM      VIDsun        alfasun           9
029000010810     C*                  PARM      *blanks       esito             1
029100010810     C*                  PARM      'A'           operazione        1
029200010810     C*                  PARM                    alfaeml          78
029300010810     C*                  IF        %error or esito = '1'
029400010810     C*                  MOVEL     '1'           ERRL00
029500010810     C*                  MOVEL     MSG(10)       DSMSMS
029600010810     C*                  ELSE
029700010810     C*                  IF        esito = '2'
029800010810     C*                  MOVEL     '1'           ERRL00
029900010810     C*                  MOVEL     MSG(9)        DSMSMS
030000010810     C*                  ENDIF
030100010810     C*                  ENDIF
030200970526     C*
030300970526     C* IMPOSTA I CAMPI DEL RECORD DA SCRIVERE
030400010810     C*    ERRL00        IFEQ      '0'                                          *NO ERRORI
030500001121xxx**C*                  MOVEL     '0'           VIDinv
030600001020     C                   EXSR      IMPabl                                       *IMPOSTA I CAMPI
030700001020     C                   UPDATE    tiabl000
030800001020     C                   MOVEL     MSG(3)        s32MSG
030900970526     C                   MOVEL     '1'           FINL00                         *FINE PGM
031000010810     C*                  ENDIF
031100001020     C                   ENDIF
031200970526     C*
031300970526     C                   ENDSR
031400010810     C*------------------------------------------------------------------------*
031500010810     C* MODIFIEDP - OPERAZIONI DI MODIFICA AD USO EDP
031600010810     C*------------------------------------------------------------------------*
031700010810     C     MODIFIEDP     BEGSR
031800010810     C*
031900010810     C* CONTROLLA I CAMPI IMMESSI
032000010810     C                   EXSR      DECODI
032100010810     C*
032200010810     C* LEGGE IL RECORD PERL'AGGIORNAMENTO
032300010810     C     ERRL00        IFEQ      '0'                                          *NO ERRORI
032400010810     C                   EXSR      LETabl
032500010810     C*
032600100907     C                   MOVEL     VIDSGIANAM    alfaute
032700010810     C                   EVAL      alfaeml = VIDem1 + VIDem2
032800010810     C* EFFETTUO CONTROLLI ED ESEGUO SERIE COMANDI REMOTI SU RAGIONE SOCIALE E RESPONSABILE
032900010810     C                   Z-ADD     *zeros        pos
033000010810     C                   EVAL      pos = %scan(api:VDDksu)
033100010810     C                   DOW       pos > *zeros
033200010810     C                   EVAL      VDDksu = %replace(' ':VDDksu:pos)
033300010810     C                   Z-ADD     *zeros        pos
033400010810     C                   EVAL      pos = %scan(api:VDDksu)
033500010810     C                   ENDDO
033600010810     C*
033700010810     C                   Z-ADD     *zeros        pos
033800010810     C                   EVAL      pos = %scan(api:VIDrsp)
033900010810     C                   DOW       pos > *zeros
034000010810     C                   EVAL      VIDrsp = %replace(' ':VIDrsp:pos)
034100010810     C                   Z-ADD     *zeros        pos
034200010810     C                   EVAL      pos = %scan(api:VIDrsp)
034300010810     C                   ENDDO
034400010810     C*
034500010810     C                   CALL(e)   'TIS730R1'
034600100906     C                   PARM                    alfaute          40
034700010810     C                   PARM      VIDpwd        alfapwd          20
034800010810     C                   PARM      VDDksu        alfarag          20
034900010810     C                   PARM      VIDrsp        alfarsp          30
035000010810     C                   PARM      VIDsun        alfasun           9
035100010810     C                   PARM      *blanks       esito             1
035200010810     C                   PARM      'A'           operazione        1
035300010810     C                   PARM                    alfaeml          78
035400010810     C                   IF        %error or esito = '1'
035500010810     C                   MOVEL     '1'           ERRL00
035600010810     C                   MOVEL     MSG(10)       DSMSMS
035700010810     C                   ELSE
035800010810     C                   IF        esito = '2'
035900010810     C                   MOVEL     '1'           ERRL00
036000010810     C                   MOVEL     MSG(9)        DSMSMS
036100010810     C                   ENDIF
036200010810     C                   ENDIF
036300010810     C*
036400010810     C* IMPOSTA I CAMPI DEL RECORD DA SCRIVERE
036500010810     C     ERRL00        IFEQ      '0'                                          *NO ERRORI
036600010810xxx**C*                  MOVEL     '0'           VIDinv
036700010810     C                   EXSR      IMPabl                                       *IMPOSTA I CAMPI
036800010810     C                   UPDATE    tiabl000
036900010810     C                   MOVEL     MSG(3)        s32MSG
037000010810     C                   MOVEL     '1'           FINL00                         *FINE PGM
037100010810     C                   ENDIF
037200010810     C                   ENDIF
037300010810     C*
037400010810     C                   ENDSR
037500970526     C*------------------------------------------------------------------------*
037600001020     C* LETabl - LEGGE IL RECORD PERL'AGGIORNAMENTO
037700970526     C*------------------------------------------------------------------------*
037800001020     C     LETabl        BEGSR
037900970526     C*
038000001020     C                   MOVEL     s32sun        KABsun
038100001020     C                   MOVEL     s32isv        KABisv
038200001020     C                   MOVEL     s32tip        KABtip
038300090707     C     KEYabl        CHAIN     tiabl01L                           9998
038400001020     C     *IN98         IFEQ      *ON                                          *ALLOCATO
038500970526     C                   MOVEL     '1'           ERRL00                         *ERRORE
038600001020     C                   MOVEL     MSG(7)        s32MSG                         *MESSAGGIO
038700970526     C                   MOVEL     '1'           FINL00                         *FINE PGM
038800001020     C                   ENDIF
038900001020     C     *IN99         IFEQ      *ON                                          *INSESISTENTE
039000970526     C                   MOVEL     '1'           ERRL00                         *ERRORE
039100001020     C                   MOVEL     MSG(6)        s32MSG                         *MESSAGGIO
039200970526     C                   MOVEL     '1'           FINL00                         *FINE PGM
039300001020     C                   ENDIF
039400970526     C*
039500970526     C                   ENDSR
039600001020     C*------------------------------------------------------------------------*
039700001024     C* DECODI - CONTROLLA CAMPI E DECODIFICA
039800001020     C*------------------------------------------------------------------------*
039900001020     C     DECODI        BEGSR
040000001024     C*
040100001024     C* Utente Strategi
040200001024     C     ERRL00        IFEQ      '0'
040300001024     C     VIDsun        IFNE      *blanks
040400001024     C                   ELSE
040500001024     C                   MOVEL     '1'           ERRL00
040600001024     C                   SETON                                        60
040700001024     C                   MOVEL     MSG(11)       DSMSMS
040800001024     C                   ENDIF
040900001024     C                   ENDIF
041000001020     C*
041100001020     C* Controllo del codice
041200001020     C     ERRL00        IFEQ      '0'
041300001024     C     VIDksu        IFNE      *blanks
041400001020     C     VIDksu        ANDNE     *zeros
041500001020     c                   EXSR      cntksc
041600001020     C                   ELSE
041700001020     C                   MOVEL     '1'           ERRL00
041800001020     C                   SETON                                        61
041900001024     C                   MOVEL     MSG(11)       DSMSMS
042000001020     C                   ENDIF
042100001020     C                   ENDIF
042200001020     C*
042300001020     C* Tipo servizio
042400001020     C     ERRL00        IFEQ      '0'
042500001024     C     VIDisv        IFNE      *blanks
042600001020     c                   EXSR      cntisv
042700001020     C                   ELSE
042800001020     C                   MOVEL     '1'           ERRL00
042900001020     C                   SETON                                        64
043000001024     C                   MOVEL     MSG(11)       DSMSMS
043100001020     C                   ENDIF
043200001020     C                   ENDIF
043300001024     C*
043400170503     C* Tipo abilitazione per invio
043500001024     C     ERRL00        IFEQ      '0'
043600001024     C     VIDtip        IFNE      *blanks
043700170503     c                   EXSR      cnttip
043800001024     C                   ELSE
043900001024     C                   MOVEL     '1'           ERRL00
044000001024     C                   SETON                                        65
044100001024     C                   MOVEL     MSG(11)       DSMSMS
044200001024     C                   ENDIF
044300001024     C                   ENDIF
044400001024     C*
044500001024     C* Password iniziale
044600001024     C     ERRL00        IFEQ      '0'
044700001024     C     VIDpwd        IFNE      *blanks
044800001024     C                   ELSE
044900001024     C                   MOVEL     '1'           ERRL00
045000001024     C                   SETON                                        66
045100001024     C                   MOVEL     MSG(11)       DSMSMS
045200001024     C                   ENDIF
045300001024     C                   ENDIF
045400100503     C*
045500100503     C* Password => caratteri consentiti
045600100503     C     ERRL00        IFEQ      '0'
045700100503     C                   Z-ADD     1             pos               2 0
045800100503     C                   FOR       pos = 1 to %len(%trim(VIDpwd))
045900100503     C                   IF        %lookup(%subst(%trim(VIDpwd):pos:1):valAry)
046000100503     C                             = *zeros
046100100503     C                   MOVEL     '1'           ERRL00
046200100503     C                   SETON                                        66
046300100503     C                   MOVEL     MSG(16)       DSMSMS
046400100503     C                   ENDIF
046500100503     C                   ENDFOR
046600100503     C                   ENDIF
046700001024     C*
046800010313     C* Responsabile referente
046900001024     C     ERRL00        IFEQ      '0'
047000001024     C     VIDrsp        IFNE      *blanks
047100010313     C                   Z-ADD     *zeros        pos               2 0
047200010313     C                   EVAL      pos = %scan(api:VIDrsp)
047300010313     C                   IF        pos > 0
047400010313     C                   MOVEL     '1'           ERRL00
047500010313     C                   MOVEL     MSG(14)       DSMSMS
047600010313     C                   ENDIF
047700001024     C                   ELSE
047800001024     C                   MOVEL     '1'           ERRL00
047900001024     C                   SETON                                        62
048000001024     C                   MOVEL     MSG(11)       DSMSMS
048100001024     C                   ENDIF
048200001024     C                   ENDIF
048300001024     C*
048400001024     C* E-mail
048500001024     C     ERRL00        IFEQ      '0'
048600001024     C                   IF        VIDem1+VIDem2 <> *blanks
048700001120     C* Lancio utility di controllo formale indirizzo e-mail
048800001120     C                   CLEAR                   DSEMAIL
048900001120     C                   EVAL      §EMLINDI = VIDem1+VIDem2
049000001120     C                   CALL      'XEMAIL'
049100001120     C                   PARM                    DSEMAIL
049200001120     C                   IF        §EMLERRO = '0'
049300001120     C                   EVAL      VIDem1 = %subst(§EMLINDO:1:49)
049400001120     C                   EVAL      VIDem2 = %subst(§EMLINDO:50:49)
049500001120     C                   ELSE
049600001120     C                   MOVEL     '1'           ERRL00
049700001120     C                   SETON                                        63
049800001120     C                   MOVEL     §EMLMSGO      DSMSMS
049900001120     C                   ENDIF
050000001120     C                   ELSE
050100001024     C                   MOVEL     '1'           ERRL00
050200001024     C                   SETON                                        63
050300001024     C                   MOVEL     MSG(11)       DSMSMS
050400001024     C                   ENDIF
050500001024     C                   ENDIF
050600060607     C*
050700060607     C* Lingua
050800060607     C     ERRL00        IFEQ      '0'
050900060607     c                   EXSR      cntLng
051000060607     C                   ENDIF
051100001020     C*
051200001020     C                   ENDSR
051300001020     C*------------------------------------------------------------------------*
051400001020     C* CNTKSC - CONTROLLA ESISTENZA CLIENTE
051500001020     C*------------------------------------------------------------------------*
051600001020     C     CNTKSC        BEGSR
051700001020     C*
051800001020     C                   MOVE      VIDksu        wcli              7 0
051900001020     C                   CLEAR                   BS69DS
052000001020     C                   CLEAR                   ACODS
052100001020     C                   MOVEL     KNSIF         I69SIF
052200001020     C                   Z-ADD     WCLI          I69KAC
052300001020     C                   CALL      'TIBS69R'
052400001020     C                   PARM                    BS69DS
052500001020     C                   PARM                    ACODS
052600001020     C                   PARM                    INDDS
052700001020     C                   PARM                    CLPDS
052800001020     C                   PARM                    CLSDS
052900001020     C     O69ERR        IFNE      '1'
053000001020     C                   MOVEL     ACORAG        VDDksu
053100001020     C                   ELSE
053200001020     C     VIDksu        CHAIN     tianv01l
053300001020     C                   IF        %found
053400001020     C                   MOVEL     ANVDES        VDDksu
053500001020     C                   ELSE
053600001020     C                   MOVEL     *ALL'*'       VDDksu
053700001020     C                   MOVE      '1'           errl00
053800001024     C                   MOVEL     MSG(12)       dsmsms
053900001020     C                   ENDIF
054000001020     C                   ENDIF
054100001020     C*
054200001020     C                   ENDSR
054300001020     C*------------------------------------------------------------------------*
054400001020     C* CNTISV - CONTROLLA TIPO SERVIZIO
054500001020     C*------------------------------------------------------------------------*
054600001020     C     CNTISV        BEGSR
054700001020     C*
054800001020     C                   CLEAR                   TIBS02DS
054900001020     C*
055000001020     C     '?'           SCAN      VIDisv                                 30
055100001020     C*  Ricerca
055200001020     C                   eval      t02tla = 'L'
055300001020     C                   IF        *IN30 = *ON
055400001020     C                   MOVEL     'R'           t02mod
055500001020     C                   ELSE
055600001020     C                   MOVEL     'C'           t02mod
055700001020     C                   MOVEL(p)  vidisv        t02ke1
055800001020     C                   ENDIF
055900001020     C                   MOVEL     'ISV'         t02cod
056000001020     C                   MOVEL     KNSIF         t02sif
056100001020     C                   CALL      'TIBS02R'
056200001020     C                   PARM                    KPJBA
056300001020     C                   PARM                    TIBS02DS
056400001020     C*
056500001020     C                   IF        T02err = *blanks
056600001020     C                   MOVEL     T02ke1        VIDisv
056700001020     C                   MOVEL     T02uni        VDDisv
056800001020     C                   ELSE
056900001020     C                   MOVEL     '1'           errl00
057000001020     C                   SETON                                        64
057100001024     C                   MOVEL     MSG(13)       DSMSMS
057200001020     C                   ENDIF
057300001020     C*
057400001020     C                   ENDSR
057500170503     C*------------------------------------------------------------------------*
057600170503     C* CNTTIP - CONTROLLA TIPO ABILITAZIONE
057700170503     C*------------------------------------------------------------------------*
057800170503     C     CNTTIP        BEGSR
057900170503     C*
058000170503     C                   CLEAR                   TIBS02DS
058100170503     C*
058200170503     C     '?'           SCAN      VIDtip                                 30
058300170503     C*  Ricerca
058400170503     C                   eval      t02tla = 'L'
058500170503     C                   IF        *IN30 = *ON
058600170503     C                   MOVEL     'R'           t02mod
058700170503     C                   ELSE
058800170503     C                   MOVEL     'C'           t02mod
058900170503     C                   MOVEL(p)  vidtip        t02ke1
059000170503     C                   ENDIF
059100170503     C                   MOVEL     'ISS'         t02cod
059200170503     C                   MOVEL     KNSIF         t02sif
059300170503     C                   CALL      'TIBS02R'
059400170503     C                   PARM                    KPJBA
059500170503     C                   PARM                    TIBS02DS
059600170503     C*
059700170503     C                   IF        T02err = *blanks
059800170503     C                   MOVEL     T02ke1        VIDtip
059900170503     C                   MOVEL     T02uni        VDDtip
060000170503     C                   ELSE
060100170503     C                   MOVEL     '1'           errl00
060200170503     C                   SETON                                        65
060300170503     C                   MOVEL     MSG(13)       DSMSMS
060400170503     C                   ENDIF
060500170503     C*
060600170503     C                   ENDSR
060700060607     C*------------------------------------------------------------------------*
060800060607     C* CNTLNG - CONTROLLA LINGUA
060900060607     C*------------------------------------------------------------------------*
061000060607     C     CNTLNG        BEGSR
061100060607     C*
061200060925     C                   CLEAR                   vidLngDes
061300060925     C                   IF        vidLng <> *BLANK
061400060607     C     vidLng        CHAIN     azlin03l
061500060607     C                   IF        %FOUND
061600060607     C                   EVAL      vidLngDes = linDesIta
061700060607     C                   ELSE
061800060607     C                   MOVEL     '1'           errl00
061900060607     C                   SETON                                        68
062000060607     C                   MOVEL     MSG(15)       DSMSMS
062100060607     C                   ENDIF
062200060925     C                   ENDIF
062300060607     C*
062400060607     C                   ENDSR
062500970526     C*------------------------------------------------------------------------*
062600001020     C* IREVID - IMPOSTA I CAMPI DEL VIDEO DAL FILE DI DATA BASE
062700970526     C*------------------------------------------------------------------------*
062800970526     C     IREVID        BEGSR
062900001020     C*
063000001020     C                   MOVEL     ablsun        VIDsun
063100010625     C                   MOVEL     *all'0'       VIDKSU
063200010625     C                   MOVE      ablute        VIDksu
063300001020     C                   MOVEL     ablisv        VIDisv
063400001020     C                   MOVEL     ablpwd        VIDpwd
063500001020     C                   MOVEL     ablrsp        VIDrsp
063600001020     C                   EVAL      VIDem1 = %SUBST(ABLEML:1:50)
063700001020     C                   EVAL      VIDem2 = %SUBST(ABLEML:50:50)
063800001020     C                   MOVEL     abltip        VIDtip
063900001020     C                   MOVEL     ablinv        VIDinv
064000010227     C                   MOVEL     abltel        VIDtel
064100010227     C                   MOVEL     ablfax        VIDfax
064200010227     C                   Z-ADD     abldep        VIDdep
064300060607     C                   EVAL      vidLng = ablFl1.dftVasLang
064400090622     C*
064500090622     C* Reperisco l'Access Name presente sul file utenti di Strategi
064600090622     C     VIDsun        chain     SGIUS
064700090622     C                   if        %found(SGIUS)
064800090622     C                   eval      VIDsgianam = USANAM
064900090622     C                   else
065000090622     C                   eval      VIDsgianam = *blanks
065100090622     C                   endif
065200970527     C*
065300970526     C                   ENDSR
065400970526     C*------------------------------------------------------------------------*
065500970526     C* AZZVID - AZZERA I CAMPI DEL VIDEO
065600970526     C*------------------------------------------------------------------------*
065700970526     C     AZZVID        BEGSR
065800001020     C*
065900001020     C                   CLEAR                   S733D03
066000170111     C                   MOVEL     '0'           VIDinv
066100970526     C*
066200970526     C                   ENDSR
066300170111     C*------------------------------------------------------------------------*
066400170111     C* PASSPAR - ASSEGNA I PARAMETRI ININPUT A VIDEO (SE PASSATI)
066500170111     C*------------------------------------------------------------------------*
066600170111     C     PASSPAR       BEGSR
066700170111     C*
066800170111     C                   EVAL      VIDSUN = S32SUN
066900170111     C                   EVAL      VIDISV = S32ISV
067000170111     C                   EVAL      VIDTIP = S32TIP
067100170111     C                   EVAL      VIDKSU = S32KSU
067200170111     C*
067300170111     C                   ENDSR
067400970526     C*------------------------------------------------------------------------*
067500001024     C* IMPabl - IMPOSTA IL RECORD DA SCRIVERE SUL FILE
067600970526     C*------------------------------------------------------------------------*
067700001020     C     IMPabl        BEGSR
067800970526     C*
067900001020     C                   MOVEL     VIDsun        ablsun
068000001020     C                   MOVE(p)   VIDksu        ablute
068100001020     C                   MOVEL     VIDisv        ablisv
068200001020     C                   MOVEL     VIDpwd        ablpwd
068300001020     C                   MOVEL     VIDrsp        ablrsp
068400001020     C                   EVAL      ableml = VIDem1 + VIDem2
068500001020     C                   MOVEL     VIDtip        abltip
068600001020     C                   MOVEL     VIDinv        ablinv
068700010227     C                   MOVEL     VIDtel        abltel
068800010227     C                   MOVEL     VIDfax        ablfax
068900010227     C                   Z-ADD     VIDdep        abldep
069000060607     C                   EVAL      ablFl1.dftVasLang = vidLng
069100970526     C*
069200970526     C                   ENDSR
069300970526     C*------------------------------------------------------------------------*
069400970526     C* IMPFUN - IMPOSTA LE FUNZIONI DI INGRESSO
069500970526     C*------------------------------------------------------------------------*
069600970526     C     IMPFUN        BEGSR
069700970526     C*
069800970526     C* SE VISUALIZZAZIONE TOGLIE LA FUNZIONE DI AGGIORNAMENTO
069900001020     C     DEPOP1        IFEQ      'O05'
070000970526     C                   MOVEL     FUD(6)        FUDRIG                         *AGGIORNAMENTO
070100970526     C                   MOVEL     'N'           FUDATT
070200970526     C                   MOVEL     FUDRIG        FUD(6)
070300001020     C                   ENDIF
070400970526     C*
070500970526     C* GESTIONE TASTI FUNZIONALI A VIDEO
070600970526     C                   Z-ADD     *ZEROS        FUNGRU            5 0
070700970526     C                   EXSR      FUNGES
070800970526     C*
070900970526     C* IMPOSTA LA PROTEZIONE CAMPI IN BASE ALLA FUNZIONE DI ENTRATA:
071000970526     C*---
071100970526     C* 90 -> PROTEZIONE CHIAVE
071200970526     C* 91 -> PROTEZIONE TOTALE
071300970526     C*---
071400001020     C     DEPOP0        IFEQ      'P02'                                        G E S T I O N E
071500001020     C     DEPOP1        IFEQ      'F06'                                        *INSERIMENTO
071600970526     C                   SETOFF                                       90        *NESSUNA PROTEZIONE
071700970526     C                   SETOFF                                       91
071800001020     C                   ENDIF
071900001020     C     DEPOP1        IFEQ      'O02'                                        *MODIFICA
072000001020     C     DEPOP1        oreq      'O01'                                        *MODIFICA
072100970526     C                   SETON                                        90        *PROTEZIONE CHIAVE
072200970526     C                   SETOFF                                       91        *NO PROTEZIONE CAMPI
072300001020     C                   ENDIF
072400001020     C     DEPOP1        IFEQ      'O04'                                        *ANNULLAMENTO
072500970526     C     DEPOP1        OREQ      'O05'                                        *VISUALIZZAZIONE
072600970526     C     DEPOP1        OREQ      'O07'                                        *RIPRISTINO
072700970526     C                   SETON                                        90        *TUTTE LE PROTEZIONI
072800970526     C                   SETON                                        91
072900001020     C                   ENDIF
073000001020     C                   ELSE                                                   *I N T E R R O G A
073100970526     C                   SETON                                        90        *TUTTE LE PROTEZIONI
073200970526     C                   SETON                                        91
073300001020     C                   ENDIF
073400970526     C*
073500970526     C* LETTURA DEL RECORD DA AGGIORNARE -NO IN INSERIMENTO-
073600001020     C     DEPOP1        IFNE      'F06'                                        *NO INSERIMENTO
073700001020     C                   MOVEL     s32sun        KABsun
073800001020     C                   MOVEL     s32isv        KABisv
073900001020     C                   MOVEL     s32tip        KABtip
074000090707     C     KEYabl        CHAIN(n)  tiabl01L                           9998
074100001020     C     *IN99         IFEQ      *ON
074200970526     C                   MOVEL     '1'           ERRL00                         *ERRORE
074300970526     C                   MOVEL     MSG(6)        DSMSMS
074400001020     C                   ELSE
074500970526     C                   EXSR      IREVID                                       *IMPOSTA CAMPI
074600001020     C     DEPOP1        IFEQ      'O05'
074700001020     C     DEPOP1        OREQ      'O02'
074800980128     C                   EXSR      DECODI                                       *DECODIFICA CAMPI
074900980128     C                   MOVEL     '0'           ERRL00                         *TOGLIE L'ERRORE
075000001020     C                   ENDIF
075100001020     C                   ENDIF
075200001020     C                   ELSE                                                   *INSERIMENTO
075300970526     C                   EXSR      AZZVID                                       *AZZERA CAMPI
075400170111     C                   EXSR      PASSPAR                                      *PASSAGGIO PARAM.IN
075500001020     C                   ENDIF
075600970526     C*
075700970526     C* IMPOSTA LA DESCRIZIONE DELLA FUNZIONE
075800001020     C     DEPOP1        IFEQ      'O02'
075900001020     C                   MOVEL     DEC(1)        VIDFUN
076000001020     C                   ENDIF
076100001020     C     DEPOP1        IFEQ      'O03'
076200991014     C                   MOVEL     DEC(6)        VIDFUN
076300001020     C                   ENDIF
076400001020     C     DEPOP1        IFEQ      'O04'
076500970526     C                   MOVEL     DEC(2)        VIDFUN
076600001020     C                   ENDIF
076700001020     C     DEPOP1        IFEQ      'O05'
076800970526     C                   MOVEL     DEC(3)        VIDFUN
076900001020     C                   ENDIF
077000001020     C     DEPOP1        IFEQ      'O07'
077100970526     C                   MOVEL     DEC(4)        VIDFUN
077200001020     C                   ENDIF
077300001020     C     DEPOP1        IFEQ      'F06'
077400970526     C                   MOVEL     DEC(5)        VIDFUN
077500001020     C                   ENDIF
077600970526     C*
077700970526     C                   ENDSR
077800970526     C*------------------------------------------------------------------------*
077900001020     C* *INZSR - OPERAZIONI INIZIALI
078000970526     C*------------------------------------------------------------------------*
078100970526     C     *INZSR        BEGSR
078200970526     C*---
078300970526     C* RICEVIMENTO PARAMETRI
078400970526     C*---
078500970526     C     *ENTRY        PLIST
078600970612     C                   PARM                    KPJBA
078700001020     C                   MOVEL     kpjbu         tis732
078800970526     C*
078900970526     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
079000970526     C     XMSGPA        PLIST
079100970526     C                   PARM                    DSMSMS
079200970526     C                   PARM                    DSMSPG
079300970526     C                   PARM                    DSMSKE
079400970526     C                   PARM                    DSMSER
079500970526     C*
079600970526     C* IMPOSTA LA DS -TASTI PREMUTI-
079700970526     C                   CALL      'XKEY'
079800970526     C                   PARM                    DSKEY
079900970526     C*---
080000970526     C* VARIABILI NON RIFERITE AL DATA BASE
080100970526     C*---
080200970526     C     *LIKE         DEFINE    FUDFUN        FUNKEY
080300001020     C     *LIKE         DEFINE    s32OP0        DEPOP0
080400001020     C     *LIKE         DEFINE    s32OP1        DEPOP1
080500001020     C     *LIKE         DEFINE    s32OP0        WOP0
080600001020     C     *LIKE         DEFINE    s32OP1        WOP1
080700970526     C*---
080800970526     C* VARIABILI DI CONTROLLO
080900970526     C*---
081000970526     C                   MOVEL     '0'           FINL00            1            *FINE PROGRAMMA
081100970526     C                   MOVEL     '0'           F03L00            1            *PREMUTO F03
081200970526     C                   MOVEL     '0'           F12L00            1            *PREMUTO F12
081300970526     C                   MOVEL     '0'           ERRL00            1            *ERRORE
081400970526     C*
081500970526     C* IMPOSTA LA DS -INVIO MESSAGGI-
081600001020     C                   MOVEL     'TIS733R'     DSMSPG
081700001020     C                   MOVEL     'TIS733R'     MSGQUE
081800970526     C                   MOVEL     *BLANKS       MSGKEY
081900970526     C                   MOVEL     *BLANKS       DSMSMS
082000970526     C*---
082100970526     C* VARIABILI RIFERITE AL DATA BASE
082200970526     C*---
082300001020     C     *LIKE         DEFINE    ablsun        KABsun
082400001020     C     *LIKE         DEFINE    ablisv        KABisv
082500001020     C     *LIKE         DEFINE    abltip        KABtip
082600970526     C*---
082700970526     C* CHIAVI DI LETTURA
082800970526     C*---
082900001020     C* LETTURA tiabl01L - COMPLETA
083000001020     C     KEYabl        KLIST
083100001020     C                   KFLD                    KABsun
083200001020     C                   KFLD                    KABisv
083300001020     C                   KFLD                    KABtip
083400970526     C*
083500970526     C* DEPOSITA LE OPZIONI DI ENTRATA
083600001020     C                   MOVEL     s32OP0        DEPOP0                         *LIVELLO PROCEDURA
083700001020     C                   MOVEL     s32OP1        DEPOP1                         *LIVELLO PROGRAMMA
083800001020     C*
083900001020     C* TESTO SUBITO LE OPZIONI DI ENTRATA
084000010810     C                   SETOFF                                       505152
084100001020     C                   IF        DEPOP1 = 'O02'
084200010810     C                   SETON                                        5052
084300001020     C                   ELSE
084400001020     C                   IF        DEPOP1 = 'O05'
084500001020     C                   SETON                                        51
084600001020     C                   ENDIF
084700001020     C                   ENDIF
084800010227     C*
084900010227     C* ED ANCHE I PARAMETRI DI ENTRATA
085000010227     C                   IF        s32ISV = 'TD'
085100010227     C                   SETON                                        80
085200010227     C                   ENDIF
085300970526     C*
085400970526     C* IMPOSTA LE FUNZIONI DI INGRESSO
085500970526     C                   EXSR      IMPFUN
085600970526     C*
085700970526     C                   ENDSR
085800970526     C*------------------------------------------------------------------------*
085900970526     C* FUNGES - GESTIONE TASTI FUNZIONALI DEL PROGRAMMA
086000970526     C*------------------------------------------------------------------------*
086100970526     C     FUNGES        BEGSR
086200970526     C*
086300970526     C* PULISCO CAMPI DI LAVORO
086400970526     C                   Z-ADD     0             W                 5 0
086500970526     C*
086600970526     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
086700970526     C* E CARICO TABELLA FUNZIONI ATTIVE
086800970526     C     1             DO        24            I
086900970526     C                   MOVEL     FUD(I)        FUDRIG
087000970526     C*
087100970526     C* PULIZIA FUNZIONI PRECEDENTI
087200970526     C                   MOVEL     *BLANKS       FUV(I)
087300970526     C*
087400970526     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
087500970526     C* (SALTA F24)
087600970526     C     FUDATT        IFEQ      'S'
087700970526     C     FUDFUN        ANDNE     '24'
087800970526     C                   ADD       1             W
087900970526     C                   MOVEL     FUDDES        FUV(W)
088000970526     C                   END
088100970526     C                   END                                                    DO WHILE
088200970526     C*
088300970526     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
088400970526     C* ALTRIMENTI DISATTIVO F24
088500970526     C     1             DO        24            I
088600970526     C                   MOVEL     FUD(I)        FUDRIG
088700970526     C*
088800970526     C     FUDFUN        IFEQ      '24'
088900970526     C     W             IFGT      6
089000970526     C                   MOVEL     'S'           FUDATT
089100970526     C                   ELSE
089200970526     C                   MOVEL     *BLANKS       FUDATT
089300970526     C                   END                                                    W GT 8
089400970526     C                   MOVEL     FUDRIG        FUD(I)
089500970526     C                   END
089600970526     C                   END                                                    DO WHILE
089700970526     C*
089800970526     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
089900970526     C     W             DIV       6             I
090000970526     C                   MVR                     Z
090100970526     C*
090200970526     C     Z             IFNE      0
090300970526     C     I             ADD       1             Z
090400970526     C                   ELSE
090500970526     C                   Z-ADD     I             Z                 5 0
090600970526     C                   END
090700970526     C*
090800970526     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
090900970526     C                   ADD       1             FUNGRU
091000970526     C     FUNGRU        IFGT      Z
091100970526     C     FUNGRU        ORLT      1
091200970526     C                   Z-ADD     1             FUNGRU
091300970526     C                   END
091400970526     C*
091500970526     C* PRIMO GRUPPO DI 6 FUNZIONI
091600970526     C     FUNGRU        IFEQ      1
091700970526     C                   MOVEL     FUV(1)        FUN001
091800970526     C                   MOVEL     FUV(2)        FUN002
091900970526     C                   MOVEL     FUV(3)        FUN003
092000970526     C                   MOVEL     FUV(4)        FUN004
092100970526     C                   MOVEL     FUV(5)        FUN005
092200970526     C                   MOVEL     FUV(6)        FUN006
092300970526     C                   END
092400970526     C*
092500970526     C* SECONDO GRUPPO DI 6 FUNZIONI
092600970526     C     FUNGRU        IFEQ      2
092700970526     C                   MOVEL     FUV(7)        FUN001
092800970526     C                   MOVEL     FUV(8)        FUN002
092900970526     C                   MOVEL     FUV(9)        FUN003
093000970526     C                   MOVEL     FUV(10)       FUN004
093100970526     C                   MOVEL     FUV(11)       FUN005
093200970526     C                   MOVEL     FUV(12)       FUN006
093300970526     C                   END
093400970526     C*
093500970526     C* TERZO GRUPPO DI 6 FUNZIONI
093600970526     C     FUNGRU        IFEQ      3
093700970526     C                   MOVEL     FUV(13)       FUN001
093800970526     C                   MOVEL     FUV(14)       FUN002
093900970526     C                   MOVEL     FUV(15)       FUN003
094000970526     C                   MOVEL     FUV(16)       FUN004
094100970526     C                   MOVEL     FUV(17)       FUN005
094200970526     C                   MOVEL     FUV(18)       FUN006
094300970526     C                   END
094400970526     C*
094500970526     C* QUARTO GRUPPO DI 6 FUNZIONI
094600970526     C     FUNGRU        IFEQ      4
094700970526     C                   MOVEL     FUV(19)       FUN001
094800970526     C                   MOVEL     FUV(20)       FUN002
094900970526     C                   MOVEL     FUV(21)       FUN003
095000970526     C                   MOVEL     FUV(22)       FUN004
095100970526     C                   MOVEL     *BLANKS       FUN005
095200970526     C                   MOVEL     *BLANKS       FUN006
095300970526     C                   END
095400970526     C*
095500970526     C* F24 SE C'E' VIENE SEMPRE VISUALIZZATO
095600970526     C                   MOVEL     *BLANKS       FUN007
095700970526     C                   MOVEL     *BLANKS       FUN008
095800970526     C*
095900970526     C     1             DO        24            I
096000970526     C                   MOVEL     FUD(I)        FUDRIG
096100970526     C     FUDFUN        IFEQ      '24'
096200970526     C     FUDATT        ANDEQ     'S'
096300970526     C                   MOVEL     FUDDES        FUN008
096400970526     C                   END
096500970526     C                   END                                                    DO
096600970526     C*
096700970526     C                   ENDSR
096800970526     C*------------------------------------------------------------------------*
096900970526     C* FUNCON - GESTIONE TASTI FUNZIONALI DELL'UTENTE
097000970526     C*------------------------------------------------------------------------*
097100970526     C     FUNCON        BEGSR
097200970526     C*
097300970526     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
097400970526     C                   EXSR      FUNVAR
097500970526     C*
097600970526     C* PULISCO CAMPI DI LAVORO
097700970526     C                   Z-ADD     0             I                 5 0
097800970526     C                   Z-ADD     0             W                 5 0
097900970526     C*
098000970526     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
098100970526     C     FUNKEY        IFNE      *BLANKS
098200970526     C     1             DO        24            I
098300970526     C                   MOVEL     FUD(I)        FUDRIG
098400970526     C*
098500970526     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
098600970526     C     FUNKEY        IFEQ      FUDFUN
098700970526     C     FUDATT        ANDEQ     'S'
098800970526     C                   Z-ADD     I             W
098900970526     C                   END
099000970526     C                   END                                                    DO WHILE
099100970526     C*
099200970526     C* FUNZIONE NON ATTIVA (ERRORE)
099300970526     C     W             IFEQ      0
099400970526     C*
099500970526     C* EMISSIONE MESSAGGIO DI ERRORE
099600970526     C                   MOVEL     MSG(1)        DSMSMS
099700970526     C*
099800970526     C                   END                                                    W=0
099900970526     C                   END                                                    FUNKEY NE *BLANKS
100000970526     C*
100100970526     C* FUNZIONE ATTIVA (PROCEDI ...)
100200970526     C     W             IFNE      0
100300970526     C     FUNKEY        OREQ      *BLANKS
100400970526     C     FUNKEY        IFEQ      *BLANKS
100500970526     C                   EXSR      ENTGES
100600970526     C                   END
100700970526     C     FUNKEY        IFEQ      '03'
100800970526     C                   EXSR      F03GES
100900970526     C                   END
101000970526     C     FUNKEY        IFEQ      '06'
101100970526     C                   EXSR      F06GES
101200970526     C                   END
101300970526     C     FUNKEY        IFEQ      '12'
101400970526     C                   EXSR      F12GES
101500970526     C                   END
101600970526     C     FUNKEY        IFEQ      '24'
101700970526     C                   EXSR      F24GES
101800970526     C                   END
101900970526     C                   END
102000970526     C*
102100970526     C                   ENDSR
102200970526     C*------------------------------------------------------------------------*
102300970526     C* FUNVAR -TRADUCE GLI INDICATORI DI TASTO FUNZIONALE IN VARIABILE
102400970526     C*------------------------------------------------------------------------*
102500970526     C     FUNVAR        BEGSR
102600970526     C*
102700970526     C* INIZIALIZZA VARIABILE -FUNKEY-
102800970526     C*
102900970526     C* FUNKEY=*BLANKS SIGNIFICA ENTER
103000970526     C     KEY           IFEQ      ENTER
103100970526     C                   MOVEL     *BLANKS       FUNKEY
103200970526     C                   END
103300970526     C     KEY           IFEQ      F1
103400970526     C                   MOVEL     '01'          FUNKEY
103500970526     C                   END
103600970526     C     KEY           IFEQ      F2
103700970526     C                   MOVEL     '02'          FUNKEY
103800970526     C                   END
103900970526     C     KEY           IFEQ      F3
104000970526     C                   MOVEL     '03'          FUNKEY
104100970526     C                   END
104200970526     C     KEY           IFEQ      F4
104300970526     C                   MOVEL     '04'          FUNKEY
104400970526     C                   END
104500970526     C     KEY           IFEQ      F5
104600970526     C                   MOVEL     '05'          FUNKEY
104700970526     C                   END
104800970526     C     KEY           IFEQ      F6
104900970526     C                   MOVEL     '06'          FUNKEY
105000970526     C                   END
105100970526     C     KEY           IFEQ      F7
105200970526     C                   MOVEL     '07'          FUNKEY
105300970526     C                   END
105400970526     C     KEY           IFEQ      F8
105500970526     C                   MOVEL     '08'          FUNKEY
105600970526     C                   END
105700970526     C     KEY           IFEQ      F9
105800970526     C                   MOVEL     '09'          FUNKEY
105900970526     C                   END
106000970526     C     KEY           IFEQ      F10
106100970526     C                   MOVEL     '10'          FUNKEY
106200970526     C                   END
106300970526     C     KEY           IFEQ      F11
106400970526     C                   MOVEL     '11'          FUNKEY
106500970526     C                   END
106600970526     C     KEY           IFEQ      F12
106700970526     C                   MOVEL     '12'          FUNKEY
106800970526     C                   END
106900970526     C     KEY           IFEQ      F13
107000970526     C                   MOVEL     '13'          FUNKEY
107100970526     C                   END
107200970526     C     KEY           IFEQ      F14
107300970526     C                   MOVEL     '14'          FUNKEY
107400970526     C                   END
107500970526     C     KEY           IFEQ      F15
107600970526     C                   MOVEL     '15'          FUNKEY
107700970526     C                   END
107800970526     C     KEY           IFEQ      F16
107900970526     C                   MOVEL     '16'          FUNKEY
108000970526     C                   END
108100970526     C     KEY           IFEQ      F17
108200970526     C                   MOVEL     '17'          FUNKEY
108300970526     C                   END
108400970526     C     KEY           IFEQ      F18
108500970526     C                   MOVEL     '18'          FUNKEY
108600970526     C                   END
108700970526     C     KEY           IFEQ      F19
108800970526     C                   MOVEL     '19'          FUNKEY
108900970526     C                   END
109000970526     C     KEY           IFEQ      F20
109100970526     C                   MOVEL     '20'          FUNKEY
109200970526     C                   END
109300970526     C     KEY           IFEQ      F21
109400970526     C                   MOVEL     '21'          FUNKEY
109500970526     C                   END
109600970526     C     KEY           IFEQ      F22
109700970526     C                   MOVEL     '22'          FUNKEY
109800970526     C                   END
109900970526     C     KEY           IFEQ      F23
110000970526     C                   MOVEL     '23'          FUNKEY
110100970526     C                   END
110200970526     C     KEY           IFEQ      F24
110300970526     C                   MOVEL     '24'          FUNKEY
110400970526     C                   END
110500970526     C     KEY           IFEQ      ROLLUP
110600970526     C                   MOVEL     'RU'          FUNKEY
110700970526     C                   END
110800970526     C     KEY           IFEQ      HELP
110900970526     C                   MOVEL     'HP'          FUNKEY
111000970526     C                   END
111100970526     C                   ENDSR
111200970526     C*------------------------------------------------------------------------*
111300970526     C* NOTE COSTRUZIONE SCHIERE
111400970526     C*------------------------------------------------------------------------*
111500970526     C*
111600970526     C* -MSG-MESSAGGI
111700970526     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
111800970526     C* (NON TOCCARE MSG,1 E MSG,2)
111900970526     C*
112000970526     C* -CMD-COMANDI
112100970526     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
112200970526     C*
112300970526     C* -OPD-OPZIONI
112400970526     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
112500970526     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
112600970526     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
112700970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
112800970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
112900970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
113000970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
113100970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
113200970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
113300970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
113400970526     C*
113500970526     C* -FUD-FUNZIONI
113600970526     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
113700970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
113800970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
113900970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
114000970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
114100970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
114200970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
114300970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
114400970526     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
114500970526     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
114600970526     O*------------------------------------------------------------------------*
114700970526** DEC - DECODIFICHE
114800970526    MODIFICA                                                      1
114900970526  ANNULLAMENTO                                                    2
115000970526VISUALIZZAZIONE                                                   3
115100970526   RIPRISTINO                                                     4
115200970526  INSERIMENTO                                                     5
115300991014     COPIA                                                        6
115400970526** MSG - MESSAGGI
115500001020TIS733R- Tasto funzionale non gestito                                          1
115600001020TIS733R- Inserimento effettuato                                                2
115700001020TIS733R- Modifica effettuata                                                   3
115800001020TIS733R- Annullamento effettuato                                               4
115900001020TIS733R- Ripristino effettuato                                                 5
116000001020TIS733R- Richiesta funzione per record inesistente                             6
116100001020TIS733R- Record momentaneamente allocato: riprovare più tardi                  7
116200001020TIS733R- Richiesta funzione per record esistente                               8
116300001116TIS733R- Cliente non iscritto in STRATEGI                                      9
116400001116TIS733R- Possibile errore remoto o di comunicazione                            10
116500001024TIS733R- Campo obbligatorio.                                                   11
116600001024TIS733R- Codice cliente errato o inesistente.                                  12
116700001024TIS733R- Tipo servizio errato o inesistente.                                   13
116800010313TIS733R- Presenti caratteri non validi (togliere gli apici).                   14
116900060607TIS733R- Lingua non ammessa per il sito VAS.                                   15
117000100503TIS733R- Caratteri non validi nella password. Inserire unicamente: A-Z  0-9    16
117100001020TIS733R-                                                                       17
117200001020TIS733R-                                                                       18
117300001020TIS733R-                                                                       19
117400001020TIS733R-                                                                       20
117500970526** FUD - FUNZIONI
117600970526                   -        (S/N)=N                        funz.01¬
117700970526                   -        (S/N)=N                        funz.02¬
117800970526F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
117900970526                   -        (S/N)=N                        funz.04¬
118000970526                   -        (S/N)=N                        funz.05¬
118100970526F06=Aggiornamento  -        (S/N)=S                        funz.06¬
118200970527                   -        (S/N)=N                        funz.07¬
118300970527                   -        (S/N)=N                        funz.08¬
118400970527                   -        (S/N)=N                        funz.09¬
118500970527                   -        (S/N)=N                        funz.10¬
118600970526                   -        (S/N)=N                        funz.11¬
118700970526F12=Ritorno        -        (S/N)=S  <= S-FISSO            funz.12¬
118800970526                   -        (S/N)=N                        funz.13¬
118900970526                   -        (S/N)=N                        funz.14¬
119000970526                   -        (S/N)=N                        funz.15¬
119100970526                   -        (S/N)=N                        funz.16¬
119200970526                   -        (S/N)=N                        funz.17¬
119300970526                   -        (S/N)=N                        funz.18¬
119400970526                   -        (S/N)=N                        funz.19¬
119500970526                   -        (S/N)=N                        funz.20¬
119600970526                   -        (S/N)=N                        funz.21¬
119700970526                   -        (S/N)=N                        funz.22¬
119800970526                   -        (S/N)=N                        funz.23¬
119900970526F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
