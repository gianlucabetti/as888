000100970526     H*------------------------------------------------------------------------*
000200001020     H* ESTENSIONE ANAGRAFICA CLIENTI ABILITATI
000300970526     H*------------------------------------------------------------------------*
000400120530     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000500120530     h dftactgrp(*no) bnddir('UBBNDDIR')
000600120530
000700970526     F*------------------------------------------------------------------------*
000800970526     F* DATA BASE
000900970526     F*------------------------------------------------------------------------*
001000120530     FtiS737d   cf   e             WORKSTN INFDS(DEVDS1)
001100120530     F                                     SFILE(S737DMS:MSGULT)
001200970527     F*------------
001300120530     FTIVIC01I  UF A E           K DISK
001400120530     FTIVIC04I  IF   E           K DISK    rename(TIVIC000:TIVIC004)
001500001020     Ftianv01L  IF   E           K DISK
001600120531     FTABEL00F  IF   E           K DISK
001700970526     D*------------------------------------------------------------------------*
001800970526     D* SCHIERE
001900970526     D*------------------------------------------------------------------------*
002000970526     D*------------
002100970526     D* DECODIFICHE
002200970526     D*------------
002300991014     D DEC             S             66    DIM(6) CTDATA PERRCD(1)
002400970526     D*------------
002500970526     D* MESSAGGI
002600970526     D*------------
002700970527     D MSG             S             78    DIM(20) CTDATA PERRCD(1)
002800970526     D*------------
002900970526     D* DESCRIZIONI TASTI FUNZIONALI
003000970526     D*------------
003100970526     D FUD             S             80    DIM(24) CTDATA PERRCD(1)
003200970526     D*------------
003300970526     D* FUNZIONI DA CARICARE A VIDEO
003400970526     D*------------
003500970526     D FUV             S             19    DIM(24)
003600970526     D*------------------------------------------------------------------------*
003700970526     D* INPUT
003800970526     D*------------------------------------------------------------------------*
003900970526     D*------------
004000970526     D* RIDEFINIZIONE FUNZIONI VIDEO
004100970526     D*------------
004200970526     D FUDDS           DS
004300970526     D  FUDDE1                 1      1
004400970526     D  FUDFUN                 2      3
004500970526     D  FUDDE2                 4     19
004600970526     D  FUDFI0                20     34
004700970526     D  FUDATT                35     35
004800970526     D  FUDFI1                36     80
004900970526     D  FUDDES                 1     19
005000970526     D  FUDRIG                 1     80
005100970526     D*------------
005200970526     D* DS GESTIONE TASTI FUNZIONALI
005300970526     D*------------
005400970526     D DSKEY         E DS
005500970526     D DEVDS1          DS
005600970526     D  KEY                  369    369
005700970526     D*------------
005800970526     D* DS "XMSG" - GESTIONE MESSAGGI
005900970526     D*------------
006000970526     D DSMSG         E DS
006100001020     D*------------
006200001020     D* DS - REPERIMENTO TABELLE
006300001020     D*------------
006400991108     D TIBS02DS      E DS
006500970528     D*------------------
006600970526     D* DS DI PROCEDURA
006700970526     D*------------
006800120530     D tiS736        E DS                  EXTNAME(tiS736ds)
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
008500120530     D*------------
008600120530     D* SDS
008700120530     D*------------
008800120530     d Status         sds
008900120530     d  SDSpgm           *proc
009000120530     d  SDSprm           *parms
009100120530     d  SDSusr               254    263
009200010313     D*------------------
009300010313     D* VARIABILI DI WRK
009400010313     D*------------------
009500010313     D API             s              1    inz('''')
009600100503     D
009700100503     D                 ds
009800100615     D  val                          63A   INZ(' ABCDEFGHIJKLMNOPQRSTUVWXYZ0123-
009900100615     D                                     456789abcdefghijklmnopqrstuvwxyz')
010000100615     D  valAry                        1A   DIM(63)
010100100503     D                                     OVERLAY(val)
010200120530     D*------------------
010300120530     D CCMAlfa         s              8
010400120531     D wL              s              3s 0
010500120531     D wErrW01         s               n   inz(*off)
010600120531     D wEndW01         s               n   inz(*off)
010700120531     D savWErrW01      s                   like(wErrW01)
010800120531     D savVIDCCM       s                   like(VIDCCM)
010900120530     D*------------------
011000120530     D* procedure
011100120530     D/COPY GAITRASRC/SRCPROTOPR,UBFMTDATE
011200970526     C*------------------------------------------------------------------------*
011300970526     C* MAIN LINES
011400970526     C*------------------------------------------------------------------------*
011500970526     C*
011600970526     C* NON CI SONO ERRORI IN ENTRATA
011700001020     C     ERRL00        IFEQ      '0'
011800970526     C*
011900970526     C* CICLO FINO A CHE NON VIENE PREMUTO F03 O F12
012000001020     C     F03L00        DOUEQ     '1'                                          *F03 PREMUTO
012100970526     C     F12L00        OREQ      '1'                                          *F12 PREMUTO
012200970526     C     FINL00        OREQ      '1'                                          *FINE PGM
012300970526     C*
012400970526     C* EMETTE FORMATO VIDEO
012500970526     C                   EXSR      BUTFMR
012600970526     C*
012700970526     C* GESTIONE TASTI FUNZIONALI
012800970526     C                   EXSR      FUNCON
012900001020     C                   ENDDO
013000970526     C*
013100970526     C* CI SONO ERRORI IN ENTRATA
013200001020     C                   ELSE
013300120530     C                   MOVEL     '1'           S36err
013400120530     C                   MOVEL     DSMSMS        S36msg
013500001020     C                   ENDIF
013600970526     C* FINE PGM
013700970526     C                   SETON                                        LR
013800970526     C*------------------------------------------------------------------------*
013900970526     C* BUTFMR - EMISSIONE FORMATO A VIDEO
014000970526     C*------------------------------------------------------------------------*
014100970526     C     BUTFMR        BEGSR
014200970526     C*
014300970526     C* EMETTE LA TESTATA
014400120530     C                   WRITE     S737D01
014500970526     C*
014600970526     C* SPEDISCE EVENTUALE MESSAGGIO
014700970526     C                   EXSR      SNDMSG
014800970526     C*
014900970526     C* EMETTE LE FUNZIONI
015000120530     C                   WRITE     S737D02
015100970526     C*
015200970526     C* EMETTE IL VIDEO
015300120530     C                   EXFMT     S737D03
015400970526     C*
015500970526     C* REIMPOSTA FLAG DI ERRORE
015600970526     C                   MOVEL     '0'           ERRL00
015700970526     C*
015800970526     C* REIMPOSTA INDICATORI DI ERRORE
015900001024     C                   SETOFF                                       606162
016000001024     C                   SETOFF                                       636465
016100010227     C                   SETOFF                                       666780
016200060607     C                   SETOFF                                       68
016300970526     C*
016400970526     C                   ENDSR
016500970526     C*------------------------------------------------------------------------*
016600970526     C* SNDMSG - SPEDISCE EVENTUALE MESSAGGIO
016700970526     C*------------------------------------------------------------------------*
016800970526     C     SNDMSG        BEGSR
016900970526     C*
017000970526     C* PULISCE IL SFILE MESSAGGI
017100970526     C                   MOVEL     '1'           *IN40
017200120530     C                   WRITE     S737DMC
017300970526     C                   MOVEL     '0'           *IN40
017400970526     C*
017500970526     C* VISUALIZZA IL MESSAGGIO
017600970526     C                   Z-ADD     *ZEROS        MSGULT            4 0
017700970526     C                   CALL      'XMSG'        XMSGPA
017800001020     C     DSMSER        IFEQ      *ZEROS
017900970526     C                   MOVEL     DSMSKE        MSGKEY
018000001020     C                   ENDIF
018100970526     C                   Z-ADD     1             MSGULT
018200120530     C                   WRITE     S737DMS
018300120530     C                   WRITE     S737DMC
018400970526     C*
018500970526     C* MANDA MESSAGGIO A *BLANKS PER PULIRLO
018600970526     C                   MOVEL     *BLANKS       DSMSMS
018700970526     C*
018800970526     C                   ENDSR
018900970526     C*------------------------------------------------------------------------*
019000970526     C* ENTGES - GESTIONE ENTER -> INVIO
019100970526     C*------------------------------------------------------------------------*
019200970526     C     ENTGES        BEGSR
019300970526     C*
019400120622     C* CONTROLLA I CAMPI IMMESSI  SE NON VISUALIZZAZIONE
019500120622     C     DEPOP1        IFNE      'O05'
019600970526     C                   EXSR      DECODI
019700120622     C                   ENDIF
019800970526     C*
019900970526     C* SE DA VISUALIZZAZIONE CON ENTER RITORNA AL PGM CHIAMANTE
020000001020     C     DEPOP1        IFEQ      'O05'
020100970526     C                   MOVEL     '1'           FINL00
020200001020     C                   ENDIF
020300970526     C*
020400970526     C                   ENDSR
020500970526     C*------------------------------------------------------------------------*
020600970526     C* F03GES - GESTIONE F03 -> FINE
020700970526     C*------------------------------------------------------------------------*
020800970526     C     F03GES        BEGSR
020900970526     C*
021000970526     C* IMPOSTA "PREMUTO F03"
021100970526     C                   MOVEL     '1'           F03L00                         *A PROGRAMMA
021200120530     C                   MOVEL     '1'           S36F03                         *DI PROCEDURA
021300970526     C*
021400970526     C                   ENDSR
021500970526     C*------------------------------------------------------------------------*
021600970526     C* F06GES - GESTIONE F06 -> AGGIORNAMENTO
021700970526     C*------------------------------------------------------------------------*
021800970526     C     F06GES        BEGSR
021900970526     C*
022000970527     C* LANCIA LA ROUTINE DI CONFERMA INTERESSATA
022100001020     C     DEPOP1        IFEQ      'F06'
022200970526     C                   EXSR      INSERI
022300001020     C                   ENDIF
022400001020     C     DEPOP1        IFEQ      'O02'
022500970526     C                   EXSR      MODIFI
022600001020     C                   ENDIF
022700010810     C     DEPOP1        IFEQ      'O08'
022800010810     C                   EXSR      MODIFIEDP
022900010810     C                   ENDIF
023000970526     C*
023100970526     C                   ENDSR
023200970526     C*------------------------------------------------------------------------*
023300970526     C* F12GES - GESTIONE F12 -> RITORNO
023400970526     C*------------------------------------------------------------------------*
023500970526     C     F12GES        BEGSR
023600970526     C*
023700970526     C* IMPOSTA "PREMUTO F12"
023800970526     C                   MOVEL     '1'           F12L00                         *A PROGRAMMA
023900120530     C                   MOVEL     '1'           S36F12                         *DI PROCEDURA
024000970526     C*
024100970526     C                   ENDSR
024200970526     C*------------------------------------------------------------------------*
024300970526     C* F24GES - GESTIONE F24 -> ALTRE FUNZIONI
024400970526     C*------------------------------------------------------------------------*
024500970526     C     F24GES        BEGSR
024600970526     C*
024700970526     C                   EXSR      FUNGES                                       *GESTIONE FUNZIONI
024800970526     C*
024900970526     C                   ENDSR
025000970526     C*------------------------------------------------------------------------*
025100970526     C* INSERI - OPERAZIONI DI INSERIMENTO RECORD
025200970526     C*------------------------------------------------------------------------*
025300970526     C     INSERI        BEGSR
025400970526     C*
025500970526     C* CONTROLLA I CAMPI IMMESSI
025600970526     C                   EXSR      DECODI
025700970526     C*
025800970526     C* IMPOSTA I CAMPI DEL RECORD DA SCRIVERE
025900001020     C     ERRL00        IFEQ      '0'                                          *NO ERRORI
026000120530     c                   EXSR      IMPVIC
026100120530     C                   WRITE     TIVIC000
026200120530     C                   MOVEL     MSG(2)        S36MSG                         *MESSAGGIO
026300970602     C*
026400970602     C* PULISCE LA VIDEATA PER UN ALTRO INSERIMENTO
026500970602     C                   EXSR      AZZVID
026600001020     C                   ENDIF
026700970526     C*
026800970526     C                   ENDSR
026900970526     C*------------------------------------------------------------------------*
027000001116     C* MODIFI - OPERAZIONI DI MODIFICA
027100970526     C*------------------------------------------------------------------------*
027200970526     C     MODIFI        BEGSR
027300970526     C*
027400970526     C* CONTROLLA I CAMPI IMMESSI
027500970526     C                   EXSR      DECODI
027600970526     C*
027700970526     C* LEGGE IL RECORD PERL'AGGIORNAMENTO
027800001020     C     ERRL00        IFEQ      '0'                                          *NO ERRORI
027900120530     C                   EXSR      LETVIC
028000001116     C*
028100120530     C* tolgo gli apici dalla ragione sociale
028200120530     C                   Z-ADD     *zeros        pos               2 0
028300120530     C                   EVAL      pos = %scan(api:VIDRag)
028400010726     C                   DOW       pos > *zeros
028500120530     C                   EVAL      VIDRag = %replace(' ':VIDRag:pos)
028600010726     C                   Z-ADD     *zeros        pos
028700120530     C                   EVAL      pos = %scan(api:VIDRag)
028800010726     C                   ENDDO
028900010726     C*
029000970526     C* IMPOSTA I CAMPI DEL RECORD DA SCRIVERE
029100010810     C*    ERRL00        IFEQ      '0'                                          *NO ERRORI
029200120530xxx**C*                  MOVEL     '0'           VIDSts
029300120530     C                   EXSR      IMPVIC                                       *IMPOSTA I CAMPI
029400120530     C                   UPDATE    TIVIC000
029500120530     C                   MOVEL     MSG(3)        S36MSG
029600970526     C                   MOVEL     '1'           FINL00                         *FINE PGM
029700010810     C*                  ENDIF
029800001020     C                   ENDIF
029900970526     C*
030000970526     C                   ENDSR
030100010810     C*------------------------------------------------------------------------*
030200010810     C* MODIFIEDP - OPERAZIONI DI MODIFICA AD USO EDP
030300010810     C*------------------------------------------------------------------------*
030400010810     C     MODIFIEDP     BEGSR
030500010810     C*
030600010810     C* CONTROLLA I CAMPI IMMESSI
030700010810     C                   EXSR      DECODI
030800010810     C*
030900010810     C* LEGGE IL RECORD PERL'AGGIORNAMENTO
031000010810     C     ERRL00        IFEQ      '0'                                          *NO ERRORI
031100120530     C                   EXSR      LETVIC
031200010810     C*
031300120530     C* tolgo gli apici dalla ragione sociale
031400010810     C                   Z-ADD     *zeros        pos
031500120530     C                   EVAL      pos = %scan(api:VIDRag)
031600010810     C                   DOW       pos > *zeros
031700120530     C                   EVAL      VIDRag = %replace(' ':VIDRag:pos)
031800010810     C                   Z-ADD     *zeros        pos
031900120530     C                   EVAL      pos = %scan(api:VIDRag)
032000010810     C                   ENDDO
032100010810     C*
032200010810     C* IMPOSTA I CAMPI DEL RECORD DA SCRIVERE
032300010810     C     ERRL00        IFEQ      '0'                                          *NO ERRORI
032400120530xxx**C*                  MOVEL     '0'           VIDSts
032500120530     C                   EXSR      IMPVIC                                       *IMPOSTA I CAMPI
032600120530     C                   UPDATE    TIVIC000
032700120530     C                   MOVEL     MSG(3)        S36MSG
032800010810     C                   MOVEL     '1'           FINL00                         *FINE PGM
032900010810     C                   ENDIF
033000010810     C                   ENDIF
033100010810     C*
033200010810     C                   ENDSR
033300970526     C*------------------------------------------------------------------------*
033400120530     C* LETVIC - LEGGE IL RECORD PERL'AGGIORNAMENTO
033500970526     C*------------------------------------------------------------------------*
033600120530     C     LETVIC        BEGSR
033700970526     C*
033800120530     C                   MOVEL     S36CCM        VICCCM
033900120530     C                   MOVEL     S36Prg        VICPrg
034000120530     C                   MOVEL     S36DDe        VICDDe
034100120530     C     K03VIC01      CHAIN     TIVIC01I                           9998
034200001020     C     *IN98         IFEQ      *ON                                          *ALLOCATO
034300970526     C                   MOVEL     '1'           ERRL00                         *ERRORE
034400120530     C                   MOVEL     MSG(7)        S36MSG                         *MESSAGGIO
034500970526     C                   MOVEL     '1'           FINL00                         *FINE PGM
034600001020     C                   ENDIF
034700001020     C     *IN99         IFEQ      *ON                                          *INSESISTENTE
034800970526     C                   MOVEL     '1'           ERRL00                         *ERRORE
034900120530     C                   MOVEL     MSG(6)        S36MSG                         *MESSAGGIO
035000970526     C                   MOVEL     '1'           FINL00                         *FINE PGM
035100001020     C                   ENDIF
035200970526     C*
035300970526     C                   ENDSR
035400001020     C*------------------------------------------------------------------------*
035500001024     C* DECODI - CONTROLLA CAMPI E DECODIFICA
035600001020     C*------------------------------------------------------------------------*
035700001020     C     DECODI        BEGSR
035800001020     C*
035900001020     C* Controllo del codice
036000001020     C     ERRL00        IFEQ      '0'
036100120530     C     VIDCCM        IFNE      *zeros
036200001020     c                   EXSR      cntksc
036300001020     C                   ELSE
036400001020     C                   MOVEL     '1'           ERRL00
036500001020     C                   SETON                                        61
036600001024     C                   MOVEL     MSG(11)       DSMSMS
036700001020     C                   ENDIF
036800001020     C                   ENDIF
036900120530     C*
037000120530     C* Ragione sociale
037100120530     C     ERRL00        IFEQ      '0'
037200120530     C     VIDRag        IFEQ      *blanks
037300120530     C                   MOVEL     '1'           ERRL00
037400120530     C                   SETON                                        62
037500120530     C                   MOVEL     MSG(11)       DSMSMS
037600120530     C                   ENDIF
037700120530     C                   ENDIF
037800120530     C*
037900120530     C                   Z-ADD     0             wData08           8 0
038000120530     C*
038100120530     C* Data decorrenza
038200120530     C     ERRL00        IFEQ      '0'
038300120530     C     VIDDDe        IFNE      0
038400120530      /free
038500120530           wData08 = %dec(%subst(
038600120530                    UBFMTDATE_Convert(
038700120530                     %editc(VIDDDe:'X'):
038800120530                     'DDMMYYYY':
038900120530                     'YYYYMMDD') :
039000120530                     1 : 8) : 8 : 0);
039100120530      /end-free
039200120530     C* data errata
039300120530     C                   if        wData08 = VIDDDe
039400120530     C                   EVAL      *in67 = *on
039500120530     C                   MOVEL     '1'           ERRL00
039600120530     C                   MOVEL     MSG(9)        DSMSMS
039700120530     C                   ENDIF
039800120530     C*
039900120530     C                   ELSE
040000120530     C                   MOVEL     '1'           ERRL00
040100120530     C                   SETON                                        67
040200120530     C                   MOVEL     MSG(11)       DSMSMS
040300120530     C                   ENDIF
040400120530     C                   ENDIF
040500120530     C*
040600120530     C* Data scadenza
0407001205301    C     ERRL00        IFEQ      '0'
0408001205302    C     VIDDSc        IFNE      0
040900120530      /free
041000120530           wData08 = %dec(%subst(
041100120530                    UBFMTDATE_Convert(
041200120530                     %editc(VIDDSc:'X'):
041300120530                     'DDMMYYYY':
041400120530                     'YYYYMMDD') :
041500120530                     1 : 8) : 8 : 0);
041600120530      /end-free
041700120530     C* data errata
041800120530     C                   if        wData08 = VIDDSc
041900120530     C                   EVAL      *in68 = *on
042000120530     C                   MOVEL     '1'           ERRL00
042100120530     C                   MOVEL     MSG(9)        DSMSMS
042200120530     C*
0423001205303x   C                   ELSE
042400120530     C* controllo che la scadenza sia > della data corrente
0425001205304    C                   IF         %date() > %date(VIDDSc:*eur)
042600120530     C                   SETON                                        68
042700120530     C                   MOVEL     '1'           ERRL00
042800120530     C                   MOVEL     MSG(16)       DSMSMS
0429001205304x   C                   ELSE
043000120530     C* se siamo qui, entrambe le date sono corrette quindi controllo che scadenza > decorrenza
0431001205305    C                   IF         %dec(%date(VIDDDe:*eur):*iso) >=
043200120530     C                              %dec(%date(VIDDSc:*eur):*iso)
043300120530     C                   SETON                                        6768
043400120530     C                   MOVEL     '1'           ERRL00
043500120530     C                   MOVEL     MSG(15)       DSMSMS
0436001205305e   C                   ENDIF
0437001205304e   C                   ENDIF
0438001205303e   C                   ENDIF
043900120530     C*
0440001205302x   C                   ELSE
044100120530     C                   MOVEL     '1'           ERRL00
044200120530     C                   SETON                                        68
044300120530     C                   MOVEL     MSG(11)       DSMSMS
0444001205302e   C                   ENDIF
0445001205301e   C                   ENDIF
044600120530     C*
044700120530     C* se siamo qui, entrambe le date sono corrette anche tra loro
044800120530     C* controllo quindi non si sovrappongano ad altri periodi per stesso cliente
044900120530     C* ACCAVALLAMENTO DATI
045000120530     C     ERRL00        IFEQ      '0'
045100120530     C     VIDCCM        SETLL     TIVIC004
045200120530     C     VIDCCM        READE     TIVIC004                               99
045300120530     C* cicla per tutti i rcd del cliente
045400120530     C     *IN99         DOWEQ     *OFF
045500120530     C* se non siamo in immissione
045600120530     C* e il progressivo a video è diversa da quella scritta
045700120530     C                   IF        DEPOP1 <> 'F06' and
045800120530     C                             VIDPrg <> VICPrg
045900120530     C* oppure siamo in immissione
046000120530     C                             or DEPOP1 = 'F06'
046100120530     C* do errore se:
046200120530     C* se la data decorrenza a video è >= di quella scritta
046300120530     C                   IF        %dec(%date(VIDDDe:*eur):*iso) >= VICDDe and
046400120530     C* e la data decorrenza è <= della scadenza scritta
046500120530     C                             %dec(%date(VIDDDe:*eur):*iso) <= VICDSc    OR
046600120530     C* oppure la data scadenza a video è >= della decorrenza scritta
046700120530     C                             %dec(%date(VIDDSc:*eur):*iso) >= VICDDe and
046800120530     C* e la data scadenzaa video è <= della scadenza scritta
046900120530     C                             %dec(%date(VIDDSc:*eur):*iso) <= VICDSc    OR
047000120530     C* oppure la data decorrenza a video è < della decorrenza scritta
047100120530     C                             %dec(%date(VIDDDe:*eur):*iso) < VICDDe and
047200120530     C* e la data scadenza a video è > della scadenza scritta
047300120530     C                             %dec(%date(VIDDSc:*eur):*iso) > VICDSc
047400120530     C                   SETON                                        6768
047500120530     C                   MOVEL     MSG(17)       DSMSMS
047600120530     C                   MOVEL     '1'           ERRL00
047700120530     C                   LEAVE
047800120530     C                   ENDIF
047900120530     C                   ENDIF
048000120530     C     VIDCCM        READE     TIVIC004                               99
048100120530     C                   ENDDO
048200120530     C                   ENDIF
048300001024     C*
048400120530     C* Tipo cappario
048500001024     C     ERRL00        IFEQ      '0'
048600001024     C     VIDtip        IFNE      *blanks
048700120622     c                   EXSR      cntTip
048800001024     C                   ELSE
048900001024     C                   MOVEL     '1'           ERRL00
049000120530     C                   SETON                                        64
049100001024     C                   MOVEL     MSG(11)       DSMSMS
049200001024     C                   ENDIF
049300001024     C                   ENDIF
049400120530     C*
049500120530     C* Tipo invio
049600120530     C     ERRL00        IFEQ      '0'
049700120530     C     VIDisv        IFNE      *blanks
049800120530     c                   EXSR      cntisv
049900120530     C                   ELSE
050000120530     C                   MOVEL     '1'           ERRL00
050100120530     C                   SETON                                        65
050200120530     C                   MOVEL     MSG(11)       DSMSMS
050300120530     C                   ENDIF
050400120530     C                   ENDIF
050500120530     C*
050600120530     C* Stato
050700120530     C     ERRL00        IFEQ      '0'
050800120530     C     VIDSts        IFEQ      *blanks
050900120530     C                   MOVEL     '1'           ERRL00
051000120530     C                   SETON                                        66
051100120530     C                   MOVEL     MSG(11)       DSMSMS
051200120530     C                   ENDIF
051300120530     C                   ENDIF
051400001024     C*
051500001024     C* E-mail
051600001024     C     ERRL00        IFEQ      '0'
051700001024     C                   IF        VIDem1+VIDem2 <> *blanks
051800001120     C* Lancio utility di controllo formale indirizzo e-mail
051900001120     C                   CLEAR                   DSEMAIL
052000001120     C                   EVAL      §EMLINDI = VIDem1+VIDem2
052100001120     C                   CALL      'XEMAIL'
052200001120     C                   PARM                    DSEMAIL
052300001120     C                   IF        §EMLERRO = '0'
052400001120     C                   EVAL      VIDem1 = %subst(§EMLINDO:1:49)
052500001120     C                   EVAL      VIDem2 = %subst(§EMLINDO:50:49)
052600001120     C                   ELSE
052700001120     C                   MOVEL     '1'           ERRL00
052800001120     C                   SETON                                        63
052900001120     C                   MOVEL     §EMLMSGO      DSMSMS
053000001120     C                   ENDIF
053100001120     C                   ELSE
053200001024     C                   MOVEL     '1'           ERRL00
053300001024     C                   SETON                                        63
053400001024     C                   MOVEL     MSG(11)       DSMSMS
053500001024     C                   ENDIF
053600001024     C                   ENDIF
053700001020     C*
053800001020     C                   ENDSR
053900001020     C*------------------------------------------------------------------------*
054000001020     C* CNTKSC - CONTROLLA ESISTENZA CLIENTE
054100001020     C*------------------------------------------------------------------------*
054200001020     C     CNTKSC        BEGSR
054300001020     C*
054400120530     C                   MOVE      VIDCCM        wcli              7 0
054500001020     C                   CLEAR                   BS69DS
054600001020     C                   CLEAR                   ACODS
054700001020     C                   MOVEL     KNSIF         I69SIF
054800001020     C                   Z-ADD     WCLI          I69KAC
054900001020     C                   CALL      'TIBS69R'
055000001020     C                   PARM                    BS69DS
055100001020     C                   PARM                    ACODS
055200001020     C                   PARM                    INDDS
055300001020     C                   PARM                    CLPDS
055400001020     C                   PARM                    CLSDS
055500001020     C     O69ERR        IFNE      '1'
055600120530     C                   MOVEL     ACORAG        VIDRag
055700001020     C                   ELSE
055800120530     C                   EVAL      CCMAlfa = '0' + %char(VIDCCM)
055900120530     C     CCMAlfa       CHAIN     tianv01l
056000001020     C                   IF        %found
056100120530     C                   MOVEL     ANVDES        VIDRag
056200001020     C                   ELSE
056300120530     C                   MOVEL     *ALL'*'       VIDRag
056400001020     C                   MOVE      '1'           errl00
056500001024     C                   MOVEL     MSG(12)       dsmsms
056600001020     C                   ENDIF
056700001020     C                   ENDIF
056800120531     C*
056900120531     C* controllo non vincolante che il cliente esista in tab.3C
057000120531     C* se siamo in immissione
057100120531     C                   IF        ERRL00 = '0'
057200120531     C                             and DEPOP1 = 'F06'
057300120531     C* se il cliente è cambiato faccio il test solo una volta
057400120531     C                   IF        VIDCCM <> savVIDCCM
057500120531     C                   EVAL      savWErrW01 = *off
057600120531     C                   EVAL      savVIDCCM = VIDCCM
057700120531     C                   Z-ADD     1             tblKUT
057800120531     C                   MOVE(P)   '3C'          tblCOD
057900120531     C                   MOVEL(P)  VIDCCM        tblKEY
058000120531     C     K03TAB00      CHAIN     TABEL00F
058100120531     C                   IF        not %found(TABEL00F) or
058200120531     C                             tblFLG <> *blanks
058300120531     C* apro finestra con msg
058400120531     C                   EVAL      W01MSG01 = 'Cliente non esiste su tab.3C.'
058500120531     C                   EVAL      W01MSG02 =
058600120531     C                             'Confermi di volerlo salvare lo stesso?'
058700120531     C                   EXSR      ExeWdwMsg
058800120531     C                   EVAL      savWErrW01 = wErrW01
058900120531     C                   IF        wErrW01 = *on
059000120531     C                   SETON                                        61
059100120531     C                   MOVEL     *ALL'*'       VIDRag
059200120531     C                   MOVE      '1'           errl00
059300120531     C                   MOVEL     MSG(18)       dsmsms
059400120531     C                   ENDIF
059500120531     C                   ENDIF
059600120531     C* se ci sono gli estremi per il test ma il cliente non è variato, riattivo l'errore se
059700120531     C* c'era e non è stato forzato
059800120531     C                   ELSE
059900120531     C                   IF        savWErrW01 = *on
060000120531     C                   SETON                                        61
060100120531     C                   MOVEL     *ALL'*'       VIDRag
060200120531     C                   MOVE      '1'           errl00
060300120531     C                   MOVEL     MSG(18)       dsmsms
060400120531     C                   ENDIF
060500120531     C                   ENDIF
060600120531     C                   ENDIF
060700001020     C*
060800001020     C                   ENDSR
060900001020     C*------------------------------------------------------------------------*
061000120531     C* EXEWDWMSG - Esegue window per messaggi
061100001020     C*------------------------------------------------------------------------*
061200120531     C     ExeWdwMsg     BEGSR
061300001020     C*
061400120531     C                   EVAL      wErrW01 = *on
061500120531     C                   EVAL      wEndW01 = *off
061600120531     C*
061700120531     C                   DOW       wEndW01 = *off
061800120531     C                   EXFMT     S737W01
061900120531     C*
062000120531     C                   IF        Key = F6
062100120531     C                   EVAL      wErrW01 = *off
062200120531     C                   EVAL      wEndW01 = *on
062300120531     C                   ENDIF
062400120531     C*
062500120531     C                   IF        Key = F12
062600120531     C                   EVAL      wEndW01 = *on
062700120531     C                   ENDIF
062800120531     C*
062900120531     C                   ENDDO
063000120531     C*
063100120531     C                   ENDSR
063200120622     C*------------------------------------------------------------------------*
063300120622     C* CNTTIP - CONTROLLA TIPO CAPPARIO
063400120622     C*------------------------------------------------------------------------*
063500120622     C     CNTTIP        BEGSR
063600120622     C*
063700120622     C                   CLEAR                   TIBS02DS
063800120622     C*
063900120622     C     '?'           SCAN      VIDTip                                 30
064000120622     C*  Ricerca
064100120622     C                   eval      t02tla = 'L'
064200120622     C                   IF        *IN30 = *ON
064300120622     C                   MOVEL     'R'           t02mod
064400120622     C                   ELSE
064500120622     C                   MOVEL     'C'           t02mod
064600120622     C                   MOVEL(p)  vidTip        t02ke1
064700120622     C                   ENDIF
064800120622     C* ancora non è stata definita la tabella
064900120622     C                   MOVEL     'xxx'         t02cod
065000120622     C                   MOVEL     KNSIF         t02sif
065100120622     C                   CALL      'TIBS02R'
065200120622     C                   PARM                    KPJBA
065300120622     C                   PARM                    TIBS02DS
065400120622     C*
065500120622     C* ancora non è stata definita la tabella
065600120622     C                   EVAL      T02err = *blanks
065700120622     C*
065800120622     C                   IF        T02err = *blanks
065900120622     C                   MOVEL     T02ke1        VIDTip
066000120622     C                   MOVEL     T02uni        VDDTip
066100120622     C                   ELSE
066200120622     C                   MOVEL     '1'           errl00
066300120622     C                   SETON                                        64
066400120622     C                   MOVEL     MSG(19)       DSMSMS
066500120622     C                   ENDIF
066600120622     C*
066700120622     C                   ENDSR
066800120531     C*------------------------------------------------------------------------*
066900120531     C* CNTISV - CONTROLLA TIPO SERVIZIO
067000120531     C*------------------------------------------------------------------------*
067100120531     C     CNTISV        BEGSR
067200120531     C*
067300001020     C                   CLEAR                   TIBS02DS
067400001020     C*
067500001020     C     '?'           SCAN      VIDisv                                 30
067600001020     C*  Ricerca
067700001020     C                   eval      t02tla = 'L'
067800001020     C                   IF        *IN30 = *ON
067900001020     C                   MOVEL     'R'           t02mod
068000001020     C                   ELSE
068100001020     C                   MOVEL     'C'           t02mod
068200001020     C                   MOVEL(p)  vidisv        t02ke1
068300001020     C                   ENDIF
068400120622     C* ancora non è stata definita la tabella
068500120622     C                   MOVEL     'xxx'         t02cod
068600001020     C                   MOVEL     KNSIF         t02sif
068700001020     C                   CALL      'TIBS02R'
068800001020     C                   PARM                    KPJBA
068900001020     C                   PARM                    TIBS02DS
069000001020     C*
069100120622     C* ancora non è stata definita la tabella
069200120622     C                   EVAL      T02err = *blanks
069300120622     C*
069400001020     C                   IF        T02err = *blanks
069500001020     C                   MOVEL     T02ke1        VIDisv
069600001020     C                   MOVEL     T02uni        VDDisv
069700001020     C                   ELSE
069800001020     C                   MOVEL     '1'           errl00
069900120622     C                   SETON                                        65
070000001024     C                   MOVEL     MSG(13)       DSMSMS
070100001020     C                   ENDIF
070200001020     C*
070300001020     C                   ENDSR
070400970526     C*------------------------------------------------------------------------*
070500001020     C* IREVID - IMPOSTA I CAMPI DEL VIDEO DAL FILE DI DATA BASE
070600970526     C*------------------------------------------------------------------------*
070700970526     C     IREVID        BEGSR
070800001020     C*
070900120530     C                   EVAL      VIDCCM =      VICCCM
071000120530     C                   EVAL      VIDPRG =      VICPrg
071100120530     C                   EVAL      VIDDDe = %dec(%date(VICDDe:*iso):*eur)
071200120530     C                   EVAL      VIDDSc = %dec(%date(VICDSc:*iso):*eur)
071300120530     C                   EVAL      VIDRag =      VICRag
071400120530     C                   EVAL      wL= %len(VICEml)
071500120702     C                   SELECT
071600120702     C                   WHEN      wL> 50
071700120530     C                   EVAL      VIDEM1 = %SUBST(VICEml:1:50)
071800120530     C                   EVAL      VIDEM2 = %SUBST(VICEml:51:wL-50)
071900120702     C                   WHEN      wL<= 50 and wL <> 0
072000120530     C                   EVAL      VIDEM1 = %SUBST(VICEml:1:wL)
072100120530     C                   EVAL      VIDEM2 = *blank
072200120702     C                   OTHER
072300120702     C                   EVAL      VIDEM1 = *blank
072400120702     C                   EVAL      VIDEM2 = *blank
072500120702     C                   ENDSL
072600120530     C                   MOVEL     VICisv        VIDisv
072700120530     C                   MOVEL     VICtip        VIDtip
072800120530     C                   MOVEL     VICSts        VIDSts
072900120530     C                   MOVEL     VICPrf        VIDPrf
073000120530     C                   MOVEL     VICDom        VIDDom
073100970527     C*
073200970526     C                   ENDSR
073300970526     C*------------------------------------------------------------------------*
073400970526     C* AZZVID - AZZERA I CAMPI DEL VIDEO
073500970526     C*------------------------------------------------------------------------*
073600970526     C     AZZVID        BEGSR
073700001020     C*
073800120530     C                   CLEAR                   S737D03
073900120530     C* dft data decorrenza = oggi
074000120530     C                   EVAL      VIDDDe = %dec(%date():*eur)
074100120530     C* dft data scadenza = 31/12/2039
074200120530     C                   EVAL      VIDDSc = 31122039
074300120530     C* dft stato = '1'
074400120530     C                   MOVEL     1             VIDSts
074500970526     C*
074600970526     C                   ENDSR
074700970526     C*------------------------------------------------------------------------*
074800120530     C* IMPVIC - IMPOSTA IL RECORD DA SCRIVERE SUL FILE
074900970526     C*------------------------------------------------------------------------*
075000120530     C     IMPVIC        BEGSR
075100120530     C*
075200120530     C* se la richiesta è di inserimento, il progressivo deve essere il successivo
075300120530     C* per il cliente
075400120530     C     DEPOP1        IFEQ      'F06'
075500120530     C     VIDCCM        SETGT     TIVIC04I
075600120530     C                   READP     TIVIC04I                               99
075700120530     C                   IF        *in99 = *off and
075800120530     C                             VICCCM = VIDCCM
075900120530     C                   EVAL      VIDPRG = VICPrg + 1
076000120530     C                   ELSE
076100120530     C                   EVAL      VIDPRG = 1
076200120530     C                   ENDIF
076300120530     C                   ENDIF
076400120530     C                   CLEAR                   TIVIC000
076500970526     C*
076600120530     C                   EVAL      VICCCM =      VIDCCM
076700120530     C                   EVAL      VICPRG =      VIDPrg
076800120530     C                   EVAL      VICDDe = %dec(%date(VIDDDe:*eur):*iso)
076900120530     C                   EVAL      VICDSc = %dec(%date(VIDDSc:*eur):*iso)
077000120530     C                   EVAL      VICRag =      VIDRag
077100120530     C                   EVAL      VICEml = VIDEM1 + VIDEM2
077200120530     C                   MOVEL     VIDisv        VICisv
077300120530     C                   MOVEL     VIDtip        VICtip
077400120530     C                   MOVEL     VIDSts        VICSts
077500120530     C                   MOVEL     SDSusr        VICPrf
077600120530     C                   EVAL      VICDom = %timestamp()
077700970526     C*
077800120702     C* se sono stato chiamato in inserimento
077900120702     C     DEPOP1        IFEQ      'F06'
078000120702     C* sicuramente il rcd è completo
078100120702     C                   EVAL      VICCmp  = *on
078200120702     C* non so se esiste su tab.3C
078300120702     C                   EVAL      VICNo3C = *off
078400120702     C                   ENDIF
078500120702     C* se sono stato chiamato in modifica
078600120702     C     DEPOP1        IFEQ      'O02'
078700120702     C* sicuramente il rcd è completo
078800120702     C                   EVAL      VICCmp  = *off
078900120702     C* lascio lo stesso dato che c'era prima per quel che riguarda l'esistenza su tab.3c
079000120702     C                   ENDIF
079100120702     C     DEPOP1        IFEQ      'O08'
079200120702     C* sicuramente il rcd è completo
079300120702     C                   EVAL      VICCmp  = *off
079400120702     C* lascio lo stesso dato che c'era prima per quel che riguarda l'esistenza su tab.3c
079500120702     C                   ENDIF
079600120702     C*
079700120702     C*
079800970526     C                   ENDSR
079900970526     C*------------------------------------------------------------------------*
080000970526     C* IMPFUN - IMPOSTA LE FUNZIONI DI INGRESSO
080100970526     C*------------------------------------------------------------------------*
080200970526     C     IMPFUN        BEGSR
080300970526     C*
080400970526     C* SE VISUALIZZAZIONE TOGLIE LA FUNZIONE DI AGGIORNAMENTO
080500001020     C     DEPOP1        IFEQ      'O05'
080600970526     C                   MOVEL     FUD(6)        FUDRIG                         *AGGIORNAMENTO
080700970526     C                   MOVEL     'N'           FUDATT
080800970526     C                   MOVEL     FUDRIG        FUD(6)
080900001020     C                   ENDIF
081000970526     C*
081100970526     C* GESTIONE TASTI FUNZIONALI A VIDEO
081200970526     C                   Z-ADD     *ZEROS        FUNGRU            5 0
081300970526     C                   EXSR      FUNGES
081400970526     C*
081500970526     C* IMPOSTA LA PROTEZIONE CAMPI IN BASE ALLA FUNZIONE DI ENTRATA:
081600970526     C*---
081700970526     C* 90 -> PROTEZIONE CHIAVE
081800970526     C* 91 -> PROTEZIONE TOTALE
081900970526     C*---
082000001020     C     DEPOP0        IFEQ      'P02'                                        G E S T I O N E
082100001020     C     DEPOP1        IFEQ      'F06'                                        *INSERIMENTO
082200970526     C                   SETOFF                                       90        *NESSUNA PROTEZIONE
082300970526     C                   SETOFF                                       91
082400001020     C                   ENDIF
082500001020     C     DEPOP1        IFEQ      'O02'                                        *MODIFICA
082600001020     C     DEPOP1        oreq      'O01'                                        *MODIFICA
082700970526     C                   SETON                                        90        *PROTEZIONE CHIAVE
082800970526     C                   SETOFF                                       91        *NO PROTEZIONE CAMPI
082900001020     C                   ENDIF
083000001020     C     DEPOP1        IFEQ      'O04'                                        *ANNULLAMENTO
083100970526     C     DEPOP1        OREQ      'O05'                                        *VISUALIZZAZIONE
083200970526     C     DEPOP1        OREQ      'O07'                                        *RIPRISTINO
083300970526     C                   SETON                                        90        *TUTTE LE PROTEZIONI
083400970526     C                   SETON                                        91
083500001020     C                   ENDIF
083600001020     C                   ELSE                                                   *I N T E R R O G A
083700970526     C                   SETON                                        90        *TUTTE LE PROTEZIONI
083800970526     C                   SETON                                        91
083900001020     C                   ENDIF
084000970526     C*
084100970526     C* LETTURA DEL RECORD DA AGGIORNARE -NO IN INSERIMENTO-
084200001020     C     DEPOP1        IFNE      'F06'                                        *NO INSERIMENTO
084300120530     C                   MOVEL     S36CCM        VICCCM
084400120530     C                   MOVEL     S36Prg        VICPrg
084500120530     C                   MOVEL     S36DDe        VICDDe
084600120530     C     K03VIC01      CHAIN(n)  TIVIC01I                           9998
084700001020     C     *IN99         IFEQ      *ON
084800970526     C                   MOVEL     '1'           ERRL00                         *ERRORE
084900970526     C                   MOVEL     MSG(6)        DSMSMS
085000001020     C                   ELSE
085100970526     C                   EXSR      IREVID                                       *IMPOSTA CAMPI
085200120622     C* nessuna decodifica e controllo in visualizzazione  -
085300120622     C***  DEPOP1        IFEQ      'O05'
085400120622     C     DEPOP1        IFEQ      'O02'
085500980128     C                   EXSR      DECODI                                       *DECODIFICA CAMPI
085600980128     C                   MOVEL     '0'           ERRL00                         *TOGLIE L'ERRORE
085700001020     C                   ENDIF
085800001020     C                   ENDIF
085900001020     C                   ELSE                                                   *INSERIMENTO
086000970526     C                   EXSR      AZZVID                                       *AZZERA CAMPI
086100001020     C                   ENDIF
086200970526     C*
086300970526     C* IMPOSTA LA DESCRIZIONE DELLA FUNZIONE
086400001020     C     DEPOP1        IFEQ      'O02'
086500001020     C                   MOVEL     DEC(1)        VIDFUN
086600001020     C                   ENDIF
086700001020     C     DEPOP1        IFEQ      'O03'
086800991014     C                   MOVEL     DEC(6)        VIDFUN
086900001020     C                   ENDIF
087000001020     C     DEPOP1        IFEQ      'O04'
087100970526     C                   MOVEL     DEC(2)        VIDFUN
087200001020     C                   ENDIF
087300001020     C     DEPOP1        IFEQ      'O05'
087400970526     C                   MOVEL     DEC(3)        VIDFUN
087500001020     C                   ENDIF
087600001020     C     DEPOP1        IFEQ      'O07'
087700970526     C                   MOVEL     DEC(4)        VIDFUN
087800001020     C                   ENDIF
087900001020     C     DEPOP1        IFEQ      'F06'
088000970526     C                   MOVEL     DEC(5)        VIDFUN
088100001020     C                   ENDIF
088200970526     C*
088300970526     C                   ENDSR
088400970526     C*------------------------------------------------------------------------*
088500001020     C* *INZSR - OPERAZIONI INIZIALI
088600970526     C*------------------------------------------------------------------------*
088700970526     C     *INZSR        BEGSR
088800970526     C*---
088900970526     C* RICEVIMENTO PARAMETRI
089000970526     C*---
089100970526     C     *ENTRY        PLIST
089200970612     C                   PARM                    KPJBA
089300120530     C                   MOVEL     kpjbu         tiS736
089400970526     C*
089500970526     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
089600970526     C     XMSGPA        PLIST
089700970526     C                   PARM                    DSMSMS
089800970526     C                   PARM                    DSMSPG
089900970526     C                   PARM                    DSMSKE
090000970526     C                   PARM                    DSMSER
090100970526     C*
090200970526     C* IMPOSTA LA DS -TASTI PREMUTI-
090300970526     C                   CALL      'XKEY'
090400970526     C                   PARM                    DSKEY
090500970526     C*---
090600970526     C* VARIABILI NON RIFERITE AL DATA BASE
090700970526     C*---
090800970526     C     *LIKE         DEFINE    FUDFUN        FUNKEY
090900120530     C     *LIKE         DEFINE    S36OP0        DEPOP0
091000120530     C     *LIKE         DEFINE    S36OP1        DEPOP1
091100120530     C     *LIKE         DEFINE    S36OP0        WOP0
091200120530     C     *LIKE         DEFINE    S36OP1        WOP1
091300970526     C*---
091400970526     C* VARIABILI DI CONTROLLO
091500970526     C*---
091600970526     C                   MOVEL     '0'           FINL00            1            *FINE PROGRAMMA
091700970526     C                   MOVEL     '0'           F03L00            1            *PREMUTO F03
091800970526     C                   MOVEL     '0'           F12L00            1            *PREMUTO F12
091900970526     C                   MOVEL     '0'           ERRL00            1            *ERRORE
092000970526     C*
092100970526     C* IMPOSTA LA DS -INVIO MESSAGGI-
092200120530     C                   MOVEL     'TIS737R'     DSMSPG
092300120530     C                   MOVEL     'TIS737R'     MSGQUE
092400970526     C                   MOVEL     *BLANKS       MSGKEY
092500970526     C                   MOVEL     *BLANKS       DSMSMS
092600970526     C*---
092700970526     C* VARIABILI RIFERITE AL DATA BASE
092800970526     C*---
092900970526     C* CHIAVI DI LETTURA
093000970526     C*---
093100120530     C* LETTURA TIVIC01I - COMPLETA
093200120530     C     K03VIC01      KLIST
093300120530     C                   KFLD                    VICCCM
093400120530     C                   KFLD                    VICPrg
093500120530     C                   KFLD                    VICDDe
093600120530     C* LETTURA TIVIC04I
093700120530     C     K02VIC04      KLIST
093800120530     C                   KFLD                    VICCCM
093900120530     C                   KFLD                    VICDDe
094000120531     C* TABEL00F - Completa
094100120531     C     K03TAB00      KLIST
094200120531     C                   KFLD                    tblKUT
094300120531     C                   KFLD                    tblCOD
094400120531     C                   KFLD                    tblKEY
094500970526     C*
094600970526     C* DEPOSITA LE OPZIONI DI ENTRATA
094700120530     C                   MOVEL     S36OP0        DEPOP0                         *LIVELLO PROCEDURA
094800120530     C                   MOVEL     S36OP1        DEPOP1                         *LIVELLO PROGRAMMA
094900001020     C*
095000001020     C* TESTO SUBITO LE OPZIONI DI ENTRATA
095100010810     C                   SETOFF                                       505152
095200120530     C* modifica
095300001020     C                   IF        DEPOP1 = 'O02'
095400010810     C                   SETON                                        5052
095500001020     C                   ELSE
095600120530     C* visualizzazione
095700001020     C                   IF        DEPOP1 = 'O05'
095800001020     C                   SETON                                        51
095900001020     C                   ENDIF
096000001020     C                   ENDIF
096100970526     C*
096200970526     C* IMPOSTA LE FUNZIONI DI INGRESSO
096300970526     C                   EXSR      IMPFUN
096400970526     C*
096500970526     C                   ENDSR
096600970526     C*------------------------------------------------------------------------*
096700970526     C* FUNGES - GESTIONE TASTI FUNZIONALI DEL PROGRAMMA
096800970526     C*------------------------------------------------------------------------*
096900970526     C     FUNGES        BEGSR
097000970526     C*
097100970526     C* PULISCO CAMPI DI LAVORO
097200970526     C                   Z-ADD     0             W                 5 0
097300970526     C*
097400970526     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
097500970526     C* E CARICO TABELLA FUNZIONI ATTIVE
097600970526     C     1             DO        24            I
097700970526     C                   MOVEL     FUD(I)        FUDRIG
097800970526     C*
097900970526     C* PULIZIA FUNZIONI PRECEDENTI
098000970526     C                   MOVEL     *BLANKS       FUV(I)
098100970526     C*
098200970526     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
098300970526     C* (SALTA F24)
098400970526     C     FUDATT        IFEQ      'S'
098500970526     C     FUDFUN        ANDNE     '24'
098600970526     C                   ADD       1             W
098700970526     C                   MOVEL     FUDDES        FUV(W)
098800970526     C                   END
098900970526     C                   END                                                    DO WHILE
099000970526     C*
099100970526     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
099200970526     C* ALTRIMENTI DISATTIVO F24
099300970526     C     1             DO        24            I
099400970526     C                   MOVEL     FUD(I)        FUDRIG
099500970526     C*
099600970526     C     FUDFUN        IFEQ      '24'
099700970526     C     W             IFGT      6
099800970526     C                   MOVEL     'S'           FUDATT
099900970526     C                   ELSE
100000970526     C                   MOVEL     *BLANKS       FUDATT
100100970526     C                   END                                                    W GT 8
100200970526     C                   MOVEL     FUDRIG        FUD(I)
100300970526     C                   END
100400970526     C                   END                                                    DO WHILE
100500970526     C*
100600970526     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
100700970526     C     W             DIV       6             I
100800970526     C                   MVR                     Z
100900970526     C*
101000970526     C     Z             IFNE      0
101100970526     C     I             ADD       1             Z
101200970526     C                   ELSE
101300970526     C                   Z-ADD     I             Z                 5 0
101400970526     C                   END
101500970526     C*
101600970526     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
101700970526     C                   ADD       1             FUNGRU
101800970526     C     FUNGRU        IFGT      Z
101900970526     C     FUNGRU        ORLT      1
102000970526     C                   Z-ADD     1             FUNGRU
102100970526     C                   END
102200970526     C*
102300970526     C* PRIMO GRUPPO DI 6 FUNZIONI
102400970526     C     FUNGRU        IFEQ      1
102500970526     C                   MOVEL     FUV(1)        FUN001
102600970526     C                   MOVEL     FUV(2)        FUN002
102700970526     C                   MOVEL     FUV(3)        FUN003
102800970526     C                   MOVEL     FUV(4)        FUN004
102900970526     C                   MOVEL     FUV(5)        FUN005
103000970526     C                   MOVEL     FUV(6)        FUN006
103100970526     C                   END
103200970526     C*
103300970526     C* SECONDO GRUPPO DI 6 FUNZIONI
103400970526     C     FUNGRU        IFEQ      2
103500970526     C                   MOVEL     FUV(7)        FUN001
103600970526     C                   MOVEL     FUV(8)        FUN002
103700970526     C                   MOVEL     FUV(9)        FUN003
103800970526     C                   MOVEL     FUV(10)       FUN004
103900970526     C                   MOVEL     FUV(11)       FUN005
104000970526     C                   MOVEL     FUV(12)       FUN006
104100970526     C                   END
104200970526     C*
104300970526     C* TERZO GRUPPO DI 6 FUNZIONI
104400970526     C     FUNGRU        IFEQ      3
104500970526     C                   MOVEL     FUV(13)       FUN001
104600970526     C                   MOVEL     FUV(14)       FUN002
104700970526     C                   MOVEL     FUV(15)       FUN003
104800970526     C                   MOVEL     FUV(16)       FUN004
104900970526     C                   MOVEL     FUV(17)       FUN005
105000970526     C                   MOVEL     FUV(18)       FUN006
105100970526     C                   END
105200970526     C*
105300970526     C* QUARTO GRUPPO DI 6 FUNZIONI
105400970526     C     FUNGRU        IFEQ      4
105500970526     C                   MOVEL     FUV(19)       FUN001
105600970526     C                   MOVEL     FUV(20)       FUN002
105700970526     C                   MOVEL     FUV(21)       FUN003
105800970526     C                   MOVEL     FUV(22)       FUN004
105900970526     C                   MOVEL     *BLANKS       FUN005
106000970526     C                   MOVEL     *BLANKS       FUN006
106100970526     C                   END
106200970526     C*
106300970526     C* F24 SE C'E' VIENE SEMPRE VISUALIZZATO
106400970526     C                   MOVEL     *BLANKS       FUN007
106500970526     C                   MOVEL     *BLANKS       FUN008
106600970526     C*
106700970526     C     1             DO        24            I
106800970526     C                   MOVEL     FUD(I)        FUDRIG
106900970526     C     FUDFUN        IFEQ      '24'
107000970526     C     FUDATT        ANDEQ     'S'
107100970526     C                   MOVEL     FUDDES        FUN008
107200970526     C                   END
107300970526     C                   END                                                    DO
107400970526     C*
107500970526     C                   ENDSR
107600970526     C*------------------------------------------------------------------------*
107700970526     C* FUNCON - GESTIONE TASTI FUNZIONALI DELL'UTENTE
107800970526     C*------------------------------------------------------------------------*
107900970526     C     FUNCON        BEGSR
108000970526     C*
108100970526     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
108200970526     C                   EXSR      FUNVAR
108300970526     C*
108400970526     C* PULISCO CAMPI DI LAVORO
108500970526     C                   Z-ADD     0             I                 5 0
108600970526     C                   Z-ADD     0             W                 5 0
108700970526     C*
108800970526     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
108900970526     C     FUNKEY        IFNE      *BLANKS
109000970526     C     1             DO        24            I
109100970526     C                   MOVEL     FUD(I)        FUDRIG
109200970526     C*
109300970526     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
109400970526     C     FUNKEY        IFEQ      FUDFUN
109500970526     C     FUDATT        ANDEQ     'S'
109600970526     C                   Z-ADD     I             W
109700970526     C                   END
109800970526     C                   END                                                    DO WHILE
109900970526     C*
110000970526     C* FUNZIONE NON ATTIVA (ERRORE)
110100970526     C     W             IFEQ      0
110200970526     C*
110300970526     C* EMISSIONE MESSAGGIO DI ERRORE
110400970526     C                   MOVEL     MSG(1)        DSMSMS
110500970526     C*
110600970526     C                   END                                                    W=0
110700970526     C                   END                                                    FUNKEY NE *BLANKS
110800970526     C*
110900970526     C* FUNZIONE ATTIVA (PROCEDI ...)
111000970526     C     W             IFNE      0
111100970526     C     FUNKEY        OREQ      *BLANKS
111200970526     C     FUNKEY        IFEQ      *BLANKS
111300970526     C                   EXSR      ENTGES
111400970526     C                   END
111500970526     C     FUNKEY        IFEQ      '03'
111600970526     C                   EXSR      F03GES
111700970526     C                   END
111800970526     C     FUNKEY        IFEQ      '06'
111900970526     C                   EXSR      F06GES
112000970526     C                   END
112100970526     C     FUNKEY        IFEQ      '12'
112200970526     C                   EXSR      F12GES
112300970526     C                   END
112400970526     C     FUNKEY        IFEQ      '24'
112500970526     C                   EXSR      F24GES
112600970526     C                   END
112700970526     C                   END
112800970526     C*
112900970526     C                   ENDSR
113000970526     C*------------------------------------------------------------------------*
113100970526     C* FUNVAR -TRADUCE GLI INDICATORI DI TASTO FUNZIONALE IN VARIABILE
113200970526     C*------------------------------------------------------------------------*
113300970526     C     FUNVAR        BEGSR
113400970526     C*
113500970526     C* INIZIALIZZA VARIABILE -FUNKEY-
113600970526     C*
113700970526     C* FUNKEY=*BLANKS SIGNIFICA ENTER
113800970526     C     KEY           IFEQ      ENTER
113900970526     C                   MOVEL     *BLANKS       FUNKEY
114000970526     C                   END
114100970526     C     KEY           IFEQ      F1
114200970526     C                   MOVEL     '01'          FUNKEY
114300970526     C                   END
114400970526     C     KEY           IFEQ      F2
114500970526     C                   MOVEL     '02'          FUNKEY
114600970526     C                   END
114700970526     C     KEY           IFEQ      F3
114800970526     C                   MOVEL     '03'          FUNKEY
114900970526     C                   END
115000970526     C     KEY           IFEQ      F4
115100970526     C                   MOVEL     '04'          FUNKEY
115200970526     C                   END
115300970526     C     KEY           IFEQ      F5
115400970526     C                   MOVEL     '05'          FUNKEY
115500970526     C                   END
115600970526     C     KEY           IFEQ      F6
115700970526     C                   MOVEL     '06'          FUNKEY
115800970526     C                   END
115900970526     C     KEY           IFEQ      F7
116000970526     C                   MOVEL     '07'          FUNKEY
116100970526     C                   END
116200970526     C     KEY           IFEQ      F8
116300970526     C                   MOVEL     '08'          FUNKEY
116400970526     C                   END
116500970526     C     KEY           IFEQ      F9
116600970526     C                   MOVEL     '09'          FUNKEY
116700970526     C                   END
116800970526     C     KEY           IFEQ      F10
116900970526     C                   MOVEL     '10'          FUNKEY
117000970526     C                   END
117100970526     C     KEY           IFEQ      F11
117200970526     C                   MOVEL     '11'          FUNKEY
117300970526     C                   END
117400970526     C     KEY           IFEQ      F12
117500970526     C                   MOVEL     '12'          FUNKEY
117600970526     C                   END
117700970526     C     KEY           IFEQ      F13
117800970526     C                   MOVEL     '13'          FUNKEY
117900970526     C                   END
118000970526     C     KEY           IFEQ      F14
118100970526     C                   MOVEL     '14'          FUNKEY
118200970526     C                   END
118300970526     C     KEY           IFEQ      F15
118400970526     C                   MOVEL     '15'          FUNKEY
118500970526     C                   END
118600970526     C     KEY           IFEQ      F16
118700970526     C                   MOVEL     '16'          FUNKEY
118800970526     C                   END
118900970526     C     KEY           IFEQ      F17
119000970526     C                   MOVEL     '17'          FUNKEY
119100970526     C                   END
119200970526     C     KEY           IFEQ      F18
119300970526     C                   MOVEL     '18'          FUNKEY
119400970526     C                   END
119500970526     C     KEY           IFEQ      F19
119600970526     C                   MOVEL     '19'          FUNKEY
119700970526     C                   END
119800970526     C     KEY           IFEQ      F20
119900970526     C                   MOVEL     '20'          FUNKEY
120000970526     C                   END
120100970526     C     KEY           IFEQ      F21
120200970526     C                   MOVEL     '21'          FUNKEY
120300970526     C                   END
120400970526     C     KEY           IFEQ      F22
120500970526     C                   MOVEL     '22'          FUNKEY
120600970526     C                   END
120700970526     C     KEY           IFEQ      F23
120800970526     C                   MOVEL     '23'          FUNKEY
120900970526     C                   END
121000970526     C     KEY           IFEQ      F24
121100970526     C                   MOVEL     '24'          FUNKEY
121200970526     C                   END
121300970526     C     KEY           IFEQ      ROLLUP
121400970526     C                   MOVEL     'RU'          FUNKEY
121500970526     C                   END
121600970526     C     KEY           IFEQ      HELP
121700970526     C                   MOVEL     'HP'          FUNKEY
121800970526     C                   END
121900970526     C                   ENDSR
122000970526     C*------------------------------------------------------------------------*
122100970526     C* NOTE COSTRUZIONE SCHIERE
122200970526     C*------------------------------------------------------------------------*
122300970526     C*
122400970526     C* -MSG-MESSAGGI
122500970526     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
122600970526     C* (NON TOCCARE MSG,1 E MSG,2)
122700970526     C*
122800970526     C* -CMD-COMANDI
122900970526     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
123000970526     C*
123100970526     C* -OPD-OPZIONI
123200970526     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
123300970526     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
123400970526     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
123500970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
123600970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
123700970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
123800970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
123900970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
124000970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
124100970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
124200970526     C*
124300970526     C* -FUD-FUNZIONI
124400970526     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
124500970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
124600970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
124700970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
124800970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
124900970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
125000970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
125100970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
125200970526     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
125300970526     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
125400970526     O*------------------------------------------------------------------------*
125500970526** DEC - DECODIFICHE
125600120530    MODIFICA                                                      1
125700970526  ANNULLAMENTO                                                    2
125800970526VISUALIZZAZIONE                                                   3
125900970526   RIPRISTINO                                                     4
126000970526  INSERIMENTO                                                     5
126100991014     COPIA                                                        6
126200970526** MSG - MESSAGGI
126300120530TIS737R- Tasto funzionale non gestito                                          1
126400120530TIS737R- Inserimento effettuato                                                2
126500120530TIS737R- Modifica effettuata                                                   3
126600120530TIS737R- Annullamento effettuato                                               4
126700120530TIS737R- Ripristino effettuato                                                 5
126800120530TIS737R- Richiesta funzione per record inesistente                             6
126900120530TIS737R- Record momentaneamente allocato: riprovare più tardi                  7
127000120530TIS737R- Richiesta funzione per record esistente                               8
127100120530TIS737R- Data errata                                                           9
127200120530TIS737R- Possibile errore remoto o di comunicazione                            10
127300120530TIS737R- Campo obbligatorio.                                                   11
127400120530TIS737R- Codice cliente errato o inesistente.                                  12
127500120530TIS737R- Tipo invio errato o inesistente.                                      13
127600120530TIS737R- Presenti caratteri non validi (togliere gli apici).                   14
127700120530TIS737R- Scadenza <= decorrenza                                                15
127800120530TIS737R- Scadenza < data corrente                                              16
127900120530TIS737R- Periodo in sovrapposizione                                            17
128000120531TIS737R- Cliente non esiste su tab.3C                                          18
128100120622TIS737R- Tipo cappario errato o inesistente                                    19
128200120530TIS737R-                                                                       20
128300970526** FUD - FUNZIONI
128400970526                   -        (S/N)=N                        funz.01¬
128500970526                   -        (S/N)=N                        funz.02¬
128600970526F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
128700970526                   -        (S/N)=N                        funz.04¬
128800970526                   -        (S/N)=N                        funz.05¬
128900970526F06=Aggiornamento  -        (S/N)=S                        funz.06¬
129000970527                   -        (S/N)=N                        funz.07¬
129100970527                   -        (S/N)=N                        funz.08¬
129200970527                   -        (S/N)=N                        funz.09¬
129300970527                   -        (S/N)=N                        funz.10¬
129400970526                   -        (S/N)=N                        funz.11¬
129500970526F12=Ritorno        -        (S/N)=S  <= S-FISSO            funz.12¬
129600970526                   -        (S/N)=N                        funz.13¬
129700970526                   -        (S/N)=N                        funz.14¬
129800970526                   -        (S/N)=N                        funz.15¬
129900970526                   -        (S/N)=N                        funz.16¬
130000970526                   -        (S/N)=N                        funz.17¬
130100970526                   -        (S/N)=N                        funz.18¬
130200970526                   -        (S/N)=N                        funz.19¬
130300970526                   -        (S/N)=N                        funz.20¬
130400970526                   -        (S/N)=N                        funz.21¬
130500970526                   -        (S/N)=N                        funz.22¬
130600970526                   -        (S/N)=N                        funz.23¬
130700970526F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
