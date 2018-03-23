000100020715     H DECEDIT('0,') DATEDIT(*DMY.)
000200000000     H*------------------------------------------------------------------------*
000300110506     H* GESTIONE TRAINI - punti di scambio
000400000000     H*------------------------------------------------------------------------*
000500951130     F*------------------------------------------------------------------------*
000600000000     F* DATA BASE
000700000000     F*------------------------------------------------------------------------*
000800110506     FTNTLZ4D   CF   E             WORKSTN INFDS(DEVDS1)
000900110509     F                                     SFILE(TLz4DVM:MSGULT)
001000000000     FAZORG01L  IF   E           K DISK
001100000000     FAZCAE01L  IF   E           K DISK
001200000000     FTABEL00F  IF   E           K DISK
001300110509     FTnpsc01l  IF   E           K DISK
001400110607     FTntlo02l  IF   E           K DISK
001500000000     FTNTLT01L  UF A E           K DISK
001600000000     F                                     COMMIT
001700110509     FTNTLS01L  UF A E           K DISK    commit
001800000000     D*------------------------------------------------------------------------*
001900000000     D* SCHIERE
002000000000     D*------------------------------------------------------------------------*
002100000000     D*-------------------
002200000000     D* SCHIERE PER MESSAGGI, OPZIONI E FUNZIONI
002300000000     D*-------------------
002400130320     D MSG             S             78    DIM(20) CTDATA PERRCD(1)             *MESSAGGI
002500110509     D FUN             S             40    DIM(31) CTDATA PERRCD(1)             *RIGA DESCR. E ABIL.
002600000000     D*-------------------
002700000000     D* SINGOLE SCHIERE ALL'INTERNO DELLA RIGA FUNZIONI
002800000000     D*-------------------
002900000000     D FFU             S              6    DIM(31)                              *FUNZIONE
003000000000     D FDE             S             19    DIM(31)                              *DESCRIZIONE FUNZION
003100000000     D FAT             S              1    DIM(31)                              *FUNZIONE ATTIVA S/N
003200000000     D*-------------------
003300000000     D* SINGOLE SCHIERE ALL'INTERNO DELLA RIGA OPZIONI
003400000000     D*-------------------
003500000000     D OPD             S              2    DIM(24)                              *OPZIONE (ALLIN. DX)
003600000000     D OPS             S              2    DIM(24)                              *OPZIONE (ALLIN. SX)
003700000000     D ODE             S             19    DIM(24)                              *DESCRIZIONE OPZIONE
003800000000     D OAT             S              1    DIM(24)                              *OPZIONE ATTIVA S/N
003900000000     D*-------------------
004000000000     D* TABELLA DESCRIZIONI A VIDEO
004100000000     D*-------------------
004200000000     D FUV             S             19    DIM(8)                               *FUNZIONI A VIDEO
004300000000     D OPV             S             19    DIM(8)                               *OPZIONI A VIDEO
004400000000     D*-------------------
004500000000     D* TABELLA TASTI FUNZIONALI PREMUTI
004600000000     D*-------------------
004700000000     D KEYS            S              1    DIM(40)
004800000000     D*-------------------
004900000000     D* GIORNI DI PARTENZA TRAINO
005000000000     D*-------------------
005100000000     D GIO             S              1    DIM(8)                               *GIORNI (1÷8)
005200000000     D DGIO            S              5    DIM(8) CTDATA PERRCD(1)              *DESCRIZIONE SINTETI
005300000000     D DGES            S             16    DIM(8) CTDATA PERRCD(1)              *DESCRIZIONE ESTESA
005400000000     D*-------------------
005500000000     D* PROGRESSIVO GIORNI
005600000000     D*-------------------
005700000000     D DALF            S             21    DIM(1) CTDATA PERRCD(1)              *ALFABETO (1÷21)
005800000000     D ALF             S              1    DIM(21)                              *ALFABETO (1÷21)
005900000000     D*-------------------
006000000000     D* GIORNI CARICATI IN MEMORIA
006100000000     D*-------------------
006200000000     D GCM             S              1    DIM(8)                               *GIORNI (1÷8)
006300000000     D GCA             S              1    DIM(8)                               *GIORNI ANNULLATI
006400000000     D*-------------------
006500000000     D* FILIALI
006600000000     D*-------------------
006700971125     D ORG             S              3  0 DIM(500)                             *CODICE
006800971125     D FEL             S              3  0 DIM(500)                             *FILIALE ELBORATORE
006900971125     D TFP             S              3  0 DIM(500)                             *TERMINAL PARTENZA
007000971125     D TFA             S              3  0 DIM(500)                             *TERMINAL ARRIVO
007100971125     D DORG            S             30    DIM(500)                             *DESCRIZIONE
007200000518     D PORG            S              1    DIM(500)                             *S=P.O. POSTE
007300090114     D NTW             S              3    DIM(500)                             *network
007400000000     D*-------------------
007500000000     D* SCHIERE DI MEMORIZZAZIONE  - FILE -
007600000000     D*-------------------
007700000000     D***
007800000000     D* FERMATE
007900000000     D***
008000000000     D FTT             S              1    DIM(50)                              *TIPO FERMATE
008100000000     D FIL             S              3  0 DIM(50)                              *FILIALE FERMATA
008200000000     D FCA             S              1    DIM(50)                              *SCARICO IN ARRIVO
008300000000     D FAR             S              1    DIM(50)                              *CARICO IN PARTENZA
008400000000     D FMT             S              5  0 DIM(50)                              *CHILOMETRI FERMATA
008500000000     D FEM             S              4  0 DIM(50)                              *TEMPO FERMATA
008600000000     D FFV             S              1    DIM(50)                              *GIORNO FOGLIO VIAGG
008700000000     D***
008800000000     D* ORARI - 8 GIORNI -
008900000000     D***
009000000000     D*-------------------
009100000000     D* SCHIERE DI MEMORIZZAZIONE    -DOPO-
009200000000     D*-------------------
009300000000     D DTT             S              1    DIM(50)                              *TIPO FERMATA
009400000000     D DIL             S              3  0 DIM(50)                              *FILIALE FERMATA
009500000000     D DGA             S              1    DIM(50)                              *GIORNO ARRIVO
009600000000     D DHA             S              4  0 DIM(50)                              *ORA ARRIVO
009700000000     D DCA             S              1    DIM(50)                              *SCARICO IN ARRIVO
009800000000     D DGP             S              1    DIM(50)                              *GIORNO PARTENZA
009900000000     D DHP             S              4  0 DIM(50)                              *ORA PARTENZA
010000000000     D DAR             S              1    DIM(50)                              *CARICO IN PARTENZA
010100000000     D DMT             S              5  0 DIM(50)                              *CHILOMETRI FERMATA
010200000000     D DEM             S              4  0 DIM(50)                              *TEMPO FERMATA
010300000000     D DEQ             S              3  0 DIM(50)                              *SEQUENZA
010400000000     D DFV             S              1    DIM(50)                              *GIORNO FOGLIO VIAGG
010500000000     D*-------------------
010600000000     D* SCHIERE DI MEMORIZZAZIONE    -PRIMA-
010700000000     D*-------------------
010800000000     D PTT             S              1    DIM(50)                              *TIPO FERMATA
010900000000     D PIL             S              3  0 DIM(50)                              *FILIALE FERMATA
011000000000     D PGA             S              1    DIM(50)                              *GIORNO ARRIVO
011100000000     D PHA             S              4  0 DIM(50)                              *ORA ARRIVO
011200000000     D PCA             S              1    DIM(50)                              *SCARICO IN ARRIVO
011300000000     D PGP             S              1    DIM(50)                              *GIORNO PARTENZA
011400000000     D PHP             S              4  0 DIM(50)                              *ORA PARTENZA
011500000000     D PAR             S              1    DIM(50)                              *CARICO IN PARTENZA
011600000000     D PMT             S              5  0 DIM(50)                              *CHILOMETRI FERMATA
011700000000     D PEM             S              4  0 DIM(50)                              *TEMPO FERMATA
011800000000     D PEQ             S              3  0 DIM(50)                              *SEQUENZA
011900000000     D PFV             S              1    DIM(50)                              *GIORNO FOGLIO VIAGG
012000000000     D*-------------------
012100000000     D* SCHIERE DI MEMORIZZAZIONE    -LAVORO-
012200000000     D*-------------------
012300000000     D LTT             S              1    DIM(50)                              *TIPO FERMATA
012400000000     D LIL             S              3  0 DIM(50)                              *FILIALE FERMATA
012500000000     D LGA             S              1    DIM(50)                              *GIORNO ARRIVO
012600000000     D LHA             S              4  0 DIM(50)                              *ORA ARRIVO
012700000000     D LCA             S              1    DIM(50)                              *SCARICO IN ARRIVO
012800000000     D LGP             S              1    DIM(50)                              *GIORNO PARTENZA
012900000000     D LHP             S              4  0 DIM(50)                              *ORA PARTENZA
013000000000     D LAR             S              1    DIM(50)                              *CARICO IN PARTENZA
013100000000     D LMT             S              5  0 DIM(50)                              *CHILOMETRI FERMATA
013200000000     D LEM             S              4  0 DIM(50)                              *TEMPO FERMATA
013300000000     D LEQ             S              3  0 DIM(50)                              *SEQUENZA
013400000000     D LFV             S              1    DIM(50)                              *GIORNO FOGLIO VIAGG
013500000000     D*-------------------
013600000000     D* SCHIERE DI LAVORO
013700000000     D*-------------------
013800000000     D JFT             S              4    DIM(50)                              FILIALE + TIPO FERMA
013900960423     D*-------------------
014000960423     D* SCHIERE ALFA/NUMERICHE DI LAVORO
014100960423     D*-------------------
014200960424     D SA7             S              1    DIM(7)                               *ALFANUMERICA 7
014300110517
014400110526     d OLdde           s              8  0 dim(10)
014500110526     d cdde            s              8  0 dim(10)
014600110517     d cdsc            s              8  0 dim(10)
014700110509
014800110509     d Indx            s              3  0
014900110513     d modsql          s              1
015000110608     d savhhs          s                   like(vidhhs)
015100110608     d savggs          s                   like(vidggs)
015200110608     d Ktrns           s                   like(tlstrns)
015300110526     d Kgse            s                   like(tlsgse)
015400110517     d Kdde            s                   like(tlsdde)
015500110513     d SAVtrns         s                   like(vidtrns)
015600110526     d OLDtrns         s                   like(vidtrns)
015700110517     d SAVkey          s                   like(key)
015800110607     d Primcag         s                   like(tloggp)
015900110607     d Primcah         s                   like(tlohhp)
016000110607     d ULTSCAg         s                   like(tloggp)
016100110607     d ULTSCAh         s                   like(tlohhp)
016200110513     d TNTLSDS       E Ds                  extname(TNTLS00F)
016300110513     d
016400110513     d wrkgetlista     s           4096    varying
016500000000     D*------------------------------------------------------------------------*
016600000000     D* INPUT
016700000000     D*------------------------------------------------------------------------*
016800000000     D*-------------------
016900000000     D* SCHIERE DI MEMORIZZAZIONE    -FILE-          - ORARI X 8 GIORNI -
017000000000     D*-------------------
017100000000     D* Es. (4:2) + 1 = 3  /  (3 X 50) =  150
017200000000     D*-------------------
017300000000     D FGGG            DS                  OCCURS(8) INZ
017400000000     D  FGA                    1     50
017500000000     D                                     DIM(50)                              *GIORNO ARRIVO
017600000000     D  FHA                   51    200P 0
017700000000     D                                     DIM(50)                              *ORA ARRIVO
017800000000     D                                     PACKEVEN
017900000000     D  FGP                  201    250
018000000000     D                                     DIM(50)                              *GIORNO PARTENZA
018100000000     D  FHP                  251    400P 0
018200000000     D                                     DIM(50)                              *ORA PARTENZA
018300000000     D                                     PACKEVEN
018400000000     D*---------------------
018500000000     D* RIDEFINIZIONE SCHIERA -FUD- (FUNZIONI)
018600000000     D*---------------------
018700000000     D DSFUN         E DS
018800000000     D  FUNRIG                 1     40
018900000000     D*---------------------
019000000000     D* DS PER GESTIONE TASTI FUNZIONALI
019100000000     D*---------------------
019200000000     D DSKEYS        E DS
019300000000     D  KEYRIG                 1     40
019400000000     D*---------------------
019500000000     D* RIDEFINIZIONE INFDS STATO DEL PROGRAMMA
019600000000     D*---------------------
019700000000     D                SDS
019800000000     D  USER                 254    263
019900000000     D*---------------------
020000000000     D* RIDEFINIZIONE INFDS DEL FILE VIDEO
020100000000     D*---------------------
020200000000     D DEVDS1          DS
020300000000     D  KEY                  369    369
020400000000     D  CURSOR               370    371
020500000000     D  FRTSFL               378    379B 0
020600000000     D*---------------------
020700000000     D* ARCHITETTURA
020800000000     D*---------------------
020900000000     D KPJBA         E DS
021000000000     D*---------------------
021100000000     D* DS ESTERNA DI PROCEDURA
021200000000     D*---------------------
021300000000     D DS00          E DS                  EXTNAME(TNTL00DS)
021400110509     dtntlp0ds       E DS
021500000000     D*---------------------
021600000000     D* DS "XMSG" - GESTIONE MESSAGGI AL PGM
021700000000     D*---------------------
021800000000     D DSMSG         E DS
021900000000     D*---------------------
022000000000     D* DS TIPI TRAINI
022100000000     D*---------------------
022200000000     D DSTV          E DS
022300960208     D*---------------------
022400960208     D* DS 148^ DESCRIZIONE ORGANIGRAMMA
022500960208     D*---------------------
022600960208     D OG148         E DS
022700000518     D*---------------------
022800000518     D* DS X LETTURA DESCRIZIONE 143 ORGANIGRAMMA (X DPD)
022900000518     D*---------------------
023000000518     D OG143         E DS
023100971020     D*------------------
023200971020     D* DS "FNLV55R" - REPERIMENTO TERMINALs
023300971020     D*------------------
023400971020     D DSLV55        E DS                  EXTNAME(FNLV55DS)
023500130320     d
023600130320     D Dpscflo       E DS
023700020715
023800020715     d DdatiUte      e ds
023900020715     d AzuteDs       e ds                  ExtName(Azute00f)
024000020715     d Tibs34Ds      e ds                  Inz
024100090115     d Ficna2ds      e ds                  Inz
024200020715
024300000000     D*------------------
024400000000     D* DS "XSRDA8" - CONTROLLA DATA (8)
024500000000     D*------------------
024600000000     D WLBDA8          DS
024700000000     D  G08DAT                 1      8  0
024800000000     D  G08INV                 9     16  0
024900000000     D  G08ERR                17     17
025000000000     D  G08TGI                18     22  0
025100000000     D*------------------
025200000000     D* COSTANTI
025300000000     D*------------------
025400000000     D* MENU
025500000000     D DESGES          C                   CONST('** GESTIONE **')
025600000000     D DESINT          C                   CONST('INTERROGAZIONE')
025700000000     D* COMMENTI
025800000000     D DESNEW          C                   CONST('GIORNO NUOVO')
025900000000     D DESATB          C                   CONST('  ANNULLATO ')
026000000000     C*------------------------------------------------------------------------*
026100000000     C* MAIN LINE
026200000000     C*------------------------------------------------------------------------*
026300000000     C*
026400000000     C* CICLO FINO A CHE:
026500000000DO  1C     $FINE         DOUEQ     '1'
026600000000     C*
026700000000     C                   MOVEL     '0'           $ERR                           *FLAG ERRORE
026800000000     C*
026900000000     C* CONTROLLA GESTIONE VIDEO
027000110506IF  2C     $GES          IFEQ      'V1'
027100110506     C                   EXSR      GESV1                                        *SUBFILE
027200000000E   2C                   ENDIF
027300000000     C*
027400000000E   1C                   ENDDO
027500000000     C*
027600000000IF  1C     $F03          IFEQ      '1'
027700000000     C                   MOVEL     '1'           D00F03
027800000000E   1C                   ENDIF
027900000000IF  1C     $F12          IFEQ      '1'
028000000000     C                   MOVEL     '1'           D00F12
028100000000E   1C                   ENDIF
028200971020     C* OPERAZIONI FINALI
028300971020     C                   EXSR      FINSR
028400971020     C*
028500000000     C                   MOVEL     DS00          KPJBU
028600000000     C                   SETON                                        LR
028700110509     C*------------------------------------------------------------------------*
028800110509     C* CARV1  - CARICA GESTIONE PRIMO VIDEO
028900110509     C*------------------------------------------------------------------------*
029000110509     C     CARV1         BEGSR
029100110509     C*
029200110509     C* DECODIFICA IL GIORNO RICHIESTO
029300110509     C                   MOVEL     D00GSE        VCCGSE                         *GIORNO SETTIMANA
029400110509IF  1C     VCCGSE        IFNE      *BLANKS
029500110509     C                   Z-ADD     *ZEROS        J                 3 0
029600110509     C                   MOVE      VCCGSE        J
029700110509     C                   MOVEL     DGIO(J)       VCDGSE                         *DECOD. GG IN LINEA
029800110509X   1C                   ELSE
029900110509     C                   MOVEL     DGIO(8)       VCDGSE                         *DECOD. GG IN LINEA
030000110509E   1C                   ENDIF
030100110509     C*
030200110509     C                   MOVEL     'V1'          $GES                           *GESTIONE ATTUALE
030300110509     C*
030400110509     C                   EXSR      CARFUN                                       *CARICA FUNZIONI
030500110509     C                   EXSR      OUTFUN                                       *EMETTE FUNZIONI
030600110509     C*
030700110509     C                   WRITE     TLz4DVT                                      *TESTATA
030800110509     C                   WRITE     TLz4DVF                                      *FUNZIONI
030900110509     C*
031000110509     C                   ENDSR
031100110506     C*------------------------------------------------------------------------*
031200110506     C* GESV1  - GESTIONE PRIMO VIDEO
031300110506     C*------------------------------------------------------------------------*
031400110506     C     GESV1         BEGSR
031500110506     C*
031600110506IF  1C     $TESVD        IFEQ      '1'                                          *RIEMETTERE TESTATA
031700110506     C                   EXSR      CARV1                                        *CARICA VIDEO MOVIM
031800110506E   1C                   ENDIF
031900110506     C*
032000110506IF  1C     MSGULT        IFNE      *ZEROS                                       *SE CI SONO MESSAGGG
032100110506     C                   EXSR      OUTMSG                                       *EMETTE MESSAGGI
032200110506E   1C                   ENDIF
032300110506     C*
032400110506     C                   EXFMT     TLZ4DV1                                      *TEMPO DELIVERY
032500110506     C*
032600110506     C                   EXSR      CONFUN                                       *CONTROLLA TASTI FUN
032700110506     C*
032800110506IF  1C     $ERR          IFEQ      '0'                                          *TASTO ABILITATO
032900110506SL  2C                   SELECT
033000110506     C     KEY           WHENEQ    F6
033100110506     C                   EXSR      F06GES                                       *REGISTRA SU FILE
033200110506     C     KEY           WHENEQ    F9
033300110506     C                   EXSR      F09GES                                       *CAMBIA GIORNO
033400110506     C     KEY           WHENEQ    F10
033500110506     C                   EXSR      F10GES                                       *INSERIMENTO
033600110509     C     KEY           WHENEQ    F16
033700110509     C                   EXSR      F16GES                                       CANCELLA ORARI GIORN
033800110506     C     KEY           WHENEQ    ENTER
033900110506     C                   EXSR      ENTV1                                        *GEST ENTER VIDEO V1
034000110506     C                   OTHER
034100110506     C                   EXSR      FUNSTD                                       *GEST.TASTI STANDARD
034200110506E   2C                   ENDSL
034300110506E   1C                   ENDIF
034400110506     C*
034500110506     C                   ENDSR
034600110509     C*------------------------------------------------------------------------*
034700110509     C* ENTV1  - GESTIONE TASTO ENTER PRIMO VIDEO
034800110509     C*------------------------------------------------------------------------*
034900110509     C     ENTV1         BEGSR
035000110517     c                   if        key<>savkey
035100110526     c                   setoff                                       8182
035200110517     c                   eval      savkey=key
035300110517     c                   endif
035400110509     C*
035500110509     C* CONTROLLA I CAMPI IMMESSI
035600110509IF  1C     wOP1          IFNE      'O04'                                        *NO X ANNULLAMENTO
035700110509     C     wOP1          ANDNE     'O07'                                        *NO X RIPRISTINO
035800110509     C     wOP1          ANDNE     'O05'                                        *NO X VISUALIZZAZION
035900110509     C                   EXSR      DECODI
036000110509E   1C                   ENDIF
036100110509     C*
036200110509     C                   ENDSR
036300110509     C*------------------------------------------------------------------------*
036400110509     C* F06GES - GESTIONE TASTO FUNZIONALE F06 ==> CONFERMA RICHIESTA
036500110509     C*------------------------------------------------------------------------*
036600110509     C     F06GES        BEGSR
036700110509     C                   MOVEL     'F06'         D00OP1                         *LIVELLO PROGRAMMA
036800110509     C*
036900110509     C* FA' QUELLO CHE FA IL TASTO ENTER PIU' LA GESTIONE DEL F06 --->
037000110509     C* CONTROLLA CAMPI E AGGIORNA I FILES SE MODIFICATI
037100110509     C                   EXSR      ENTV1
037200110517
037300110509IF  2C     $ERR          IFEQ      '0'                                          *NO ERRORI
037400110517     c*  chiedo se devo aggioranre il traino di scambio
037500130320     c                   if        not *in81   and vidtrns>0
037600110608     c                   eval      copcop='SI'
037700110517     c                   seton                                        81
037800110526     c                   eval      copmod='MODIFICARE'
037900110525     c                   clear                   copconf
038000110525     c                   eval      copconf='F6=Conferma'
038100110517     c                   exsr      CardatCOP
038200110608     c* Se il traino da cui copiare è stato modificato--> devo anche
038300110526     c*  cancellare da questo
038400110526     c                   if        oldtrns<>vidtrns  and oldtrns>0
038500110526     c                   seton                                        82
038600110526     c                   eval      copmod2='ANNULLARE '
038700110526     c                   eval      copoldtrns=oldtrns
038800110526     c                   endif
038900110526     c
039000110517     c                   eval      $err='1'
039100110517     C                   LEAVESR
039200110517     c                   endif
039300110517     c
039400110509     C                   EXSR      AGGFIL
039500110509     C                   MOVEL     MSG(05)       $MSG
039600110509     C                   MOVEL     '1'           $FINE                          *ESCE
039700110509     C                   MOVEL     '1'           $F12                           *NO FINESTRA GIORNI
039800110509X   2C                   ELSE                                                   *ERRORI
039900110526     c                   setoff                                       8182
040000110509     C                   EXSR      CARMSG
040100110509E   2C                   ENDIF
040200110509     C*
040300110509     C                   ENDSR
040400000000     C*------------------------------------------------------------------------*
040500110509     C* F16GES - GESTIONE TASTO FUNZIONALE F16 ==> CANCELLA punto scambio
040600000000     C*------------------------------------------------------------------------*
040700110509     C     F16GES        BEGSR
040800110517     C                   EXSR      ENTV1
040900110517     c
041000110517IF  1C     $ERR          IFEQ      '1'                                          *NO ERRORI
041100110517     C                   EXSR      CARMSG
041200110517     c                   leavesr
041300110517    1c                   endif
041400110517     C
041500110517     c*  chiedo se devo aggiornare il traino di scambio
041600130320    1c                   if        not *in81  and vidtrns>0
041700110517     c                   seton                                        81
041800110517     c                   eval      copmod=' ANNULLARE'
041900110608     c                   eval      copcop='SI'
042000110525     c                   clear                   copconf
042100110525     c                   eval      copconf='F16=Annulla '
042200110517     c                   eval      $err='1'
042300110517     c                   exsr      CardatCOP
042400110517     C                   LEAVESR
042500110517    1c                   endif
042600110517
042700110509     C                   MOVEL     'F16'         D00OP1                         *LIVELLO PROGRAMMA
042800110517     c
042900110509     c     keytls        chain     tntls01l
043000110526     c                   exsr      AnnTLS
043100110517     c*
043200110517     c* Se richiesto faccio il giro sul traino di copia per annullare
043300110517    1c                   if        copcop='SI'
043400110526     c                   eval      ktrns=vidtrns
043500110517     c                   eval      kgse=d00gse
043600110517     c                   z-add     1             xx
043700110517    2c                   dow       cdde(xx)>0
043800110517     c                   eval      kdde=cdde(XX)
043900110517     c     ktlssc        chain     tntls01l
044000110526     c                   exsr      AnnTLS
044100110517     c                   add       1             xx
044200110517    2c                   enddo
044300110517     c
044400110517E   1C                   ENDIF
044500110517
044600110510     C                   MOVEL     MSG(12)       $MSG
044700110510     C                   MOVEL     '1'           $FINE                          *ESCE
044800110510     C                   MOVEL     '1'           $F12                           *NO FINESTRA GIORNI
044900110509
045000110509     c                   ENDSR
045100110517     C*------------------------------------------------------------------------*
045200110517     c* Annullamento record
045300110517     C*------------------------------------------------------------------------*
045400110526     c     AnnTLS        BEGSR
045500110517     C* GIA' TRASMESSO ANNULLA
045600110517    1c                   if        %found(tntls01l)
045700130529IF  2C**   TLsDTR        IFGT      *ZEROS
045800130529     C**                 MOVEL     'A'           TLsATB                         *ANNULLAMENTO
045900130529     C**                 Z-ADD     DATCOR        TLsDUV                         *DATA VARIAZIONE
046000130529     C**                 MOVEL     *BLANKS       TLsFTR                         *FLAG TRASMISSIONE
046100130529     C**                 UPDATE    TNTLs000
046200110517     C* NON TRASMESSO CANCELLA
046300130529X   2C**                 ELSE
046400110517     C                   DELETE    TNTLs000
046500130529E   2C**                 ENDIF
046600110517E   1C                   ENDIF
046700110517
046800110517     c                   ENDSR
046900110517     C*------------------------------------------------------------------------*
047000110517     c* Carcamento date da aggiornare
047100110517     C*------------------------------------------------------------------------*
047200110517     c     CarDatCOP     BEGSR
047300110517     c
047400110517     c                   z-add     1             xx
047500110517     c                   dow       cdde(xx) >0 and xx<=3
047600110526     C                   Z-ADD     cdde(xx)      G08INV
047700110517     C                   Z-ADD     *ZEROS        G08DAT
047800110517     C                   MOVEL     '3'           G08ERR
047900110517     C                   CALL      'XSRDA8'
048000110517     C                   PARM                    WLBDA8
048100110517     c                   if         xx=1
048200110517     C                   Z-ADD     G08DAT        copdde1
048300110517     c                   endif
048400110517     c                   if         xx=2
048500110517     C                   Z-ADD     G08DAT        copdde2
048600110517     c                   endif
048700110517     c                   if         xx=3
048800110517     C                   Z-ADD     G08DAT        copdde3
048900110517     c                   endif
049000110517
049100110517     C                   Z-ADD     cdsc(xx)      G08INV
049200110517     C                   Z-ADD     *ZEROS        G08DAT
049300110517     C                   MOVEL     '3'           G08ERR
049400110517     C                   CALL      'XSRDA8'
049500110517     C                   PARM                    WLBDA8
049600110517     c                   if         xx=1
049700110517     C                   Z-ADD     G08DAT        copdsc1
049800110517     c                   endif
049900110517     c                   if         xx=2
050000110517     C                   Z-ADD     G08DAT        copdsc2
050100110517     c                   endif
050200110517     c                   if         xx=3
050300110517     C                   Z-ADD     G08DAT        copdsc3
050400110517     c                   endif
050500110517     c
050600110517     c                   add       1             xx
050700110517     c                   enddo
050800110517     c
050900110517     c                   ENDSR
051000000000     C*------------------------------------------------------------------------*
051100000000     C* F09GES - GESTIONE TASTO FUNZIONALE F09 ===> CAMBIA GIORNO
051200000000     C*------------------------------------------------------------------------*
051300000000     C     F09GES        BEGSR
051400000000     C*
051500000000     C                   MOVEL     'F09'         D00OP1                         *LIVELLO PROGRAMMA
051600000000     C                   MOVEL     '0'           $ERR
051700110509
051800110509     c* se cambio giorno, registro quello che ho sotto
051900110509     C                   EXSR      ENTV1
052000110509IF  2C     $ERR          IFEQ      '0'                                          *NO ERRORI
052100110509     C                   EXSR      AGGFIL
052200110509X   2C                   ELSE                                                   *ERRORI
052300110509     C                   EXSR      CARMSG
052400110509     c                   leavesr
052500110509E   2C                   ENDIF
052600000000     C*
052700000000     C* LANCIA LA RICHIESTA DI UN GIORNO SUCESSIVO
052800000000     C                   Z-ADD     D00TRN        WTRN                           *NUMERO TRAINO
052900000000     C                   Z-ADD     D00DDE        WDDE                           *DATA DECORRENZA
053000000000     C                   Z-ADD     *ZEROS        WSEQ                           *SEQUENZA
053100000000     C                   MOVEL     *BLANKS       WTFM                           *TIPO FERMATA
053200000000     C                   Z-ADD     *ZEROS        WFIL                           *FILIALE FERMATA
053300000000     C                   MOVEL     *BLANKS       WGGA                           *GIORNO ARRIVO
053400000000     C                   Z-ADD     *ZEROS        WHHA                           *ORA ARRIVO
053500000000     C                   MOVEL     *BLANKS       WSCA                           *SCARICO IN ARRIVO
053600000000     C                   MOVEL     *BLANKS       WGGP                           *GIORNO PARTENZA
053700000000     C                   Z-ADD     *ZEROS        WHHP                           *ORA PARTENZA
053800000000     C                   MOVEL     *BLANKS       WCAR                           *CARICO IN PARTENZA
053900000000     C                   Z-ADD     *ZEROS        WKMT                           *CHILOMETRI FERMATA
054000000000     C                   Z-ADD     *ZEROS        WTEM                           *TEMPO FERMATA
054100000000     C                   MOVEL     *BLANKS       WGFV                           *GIORNO F.V.
054200000000     C*
054300951212     C* SE ERRORE, EMETTE UN MESSAGGIO DI AVVERTIMENTO DI PERDITA MODIFICHE
054400951212     C                   MOVEL     *BLANKS       WMSG                           *MESSAGGIO
054500951212IF  1C     $ERR          IFEQ      '1'
054600951212     C                   MOVEL     '0'           $ERR
054700130320     C                   MOVEL     MSG(19)       WMSG
054800951212E   1C                   ENDIF
054900000000     C                   MOVEL     D00OP0        WOP0                           *LIVELLO PROCEDURA
055000000000     C                   MOVEL     'F09'         WOP1                           *LIVELLO PROGRAMMA
055100110509     C                   MOVEL     'TNTL06R'     WPGM              8            *PGM DI GESTIONE
055200000000     C                   EXSR      LANPGM                                       *LANCIA PROGRAMMA
055300000000     C*
055400000000     C* AGGIORNA LE SCHIERE DI MEMORIZZAZIONE E RICARICA IL SUBFILE
055500000000IF  1C     D00F03        IFNE      '1'
055600000000IF  2C     $ERR          IFEQ      '0'
055700000000     C                   MOVEL     *BLANKS       MSGULT
055800000000     C                   MOVEL     *BLANKS       $MSG
055900110509     c                   exsr      impvid
056000000000X   2C                   ELSE
056100000000     C                   MOVEL     '0'           $ERR
056200000000     C                   EXSR      CARMSG
056300000000E   2C                   ENDIF
056400000000E   1C                   ENDIF
056500000000     C*
056600000000     C                   ENDSR
056700110509     C*------------------------------------------------------------------------*
056800110509     C* AGGFIL - Aggiornamento file TNTLS00F
056900110509     C*------------------------------------------------------------------------*
057000110509     C     AGGFIL        BEGSR
057100110517     c
057200110509     c     keytls        chain     tntls01l
057300110517     c
057400110517    1c                   if        not %found(tntls01l)
057500110509     c                   clear                   tntls000
057600110519     c                   eval      tlsdir=datcor
057700110509     c                   eval      tlstrn=d00trn
057800110509     c                   eval      tlsdde=d00dde
057900110509     c                   eval      tlsgse=d00gse
058000110517    1c                   endif
058100110525
058200110525     c                   clear                   tlsatb
058300110509     c                   eval      tlspsc=vidpsc
058400110510     c                   eval      tlstrns=vidtrns
058500110509     c                   eval      tlsggs=vidggs
058600110509     c                   eval      tlshhs=vidhhs
058700110513     c                   eval      tlsnot=vidnot1+vidnot2+vidnot3
058800110525     C                   MOVEL     *BLANKS       TLSFTR                         *FLAG TRASMISSIONE
058900110509     c
059000110517    1c                   if        not %found(tntls01l)
059100110525     C                   Z-ADD     DATCOR        TLsDIR                         *DATA INSERIMENTO
059200110525     C                   Z-ADD     *ZEROS        TLsDUV                         *DATA VARIAZIONE
059300110525     C                   Z-ADD     *ZEROS        TLsDTR                         *DATA TRASMISSIONE
059400110509     c                   write     tntls000
059500110509     c                   else
059600110519     c                   eval      tlsduv=datcor
059700110509     c                   update    tntls000
059800110517    1c                   endif
059900110509     c
060000110517     c* se devo aggiornare anche traino di copia eseguo
060100130320    1c                   if        copcop='SI' and vidtrns>0
060200110526     c                   eval      ktrns=vidtrns
060300110517     c                   eval      kgse=d00gse
060400110517     c                   z-add     1             xx
060500110517    2c                   dow       cdde(xx)>0
060600110517     c                   eval      kdde=cdde(XX)
060700110517
060800110517     c     ktlssc        chain     tntls01l
060900110517    3c                   if        not %found(tntls01l)
061000110517     c                   clear                   tntls000
061100110519     c                   eval      tlsdir=datcor
061200110517     c                   eval      tlstrn=vidtrns
061300110517     c                   eval      tlsdde=kdde
061400110517     c                   eval      tlsgse=d00gse
061500110517    3c                   endif
061600110525     c                   clear                   tlsatb
061700110517     c                   eval      tlspsc=vidpsc
061800110517     c                   eval      tlstrns=d00trn
061900110517     c                   eval      tlsggs=vidggs
062000110517     c                   eval      tlshhs=vidhhs
062100110517     c                   eval      tlsnot=vidnot1+vidnot2+vidnot3
062200110525     C                   MOVEL     *BLANKS       TLSFTR                         *FLAG TRASMISSIONE
062300110517     c
062400110517    3c                   if        not %found(tntls01l)
062500110525     C                   Z-ADD     DATCOR        TLsDIR                         *DATA INSERIMENTO
062600110525     C                   Z-ADD     *ZEROS        TLsDUV                         *DATA VARIAZIONE
062700110525     C                   Z-ADD     *ZEROS        TLsDTR                         *DATA TRASMISSIONE
062800110517     c                   write     tntls000
062900110517     c                   else
063000110519     c                   eval      tlsduv=datcor
063100110517     c                   update    tntls000
063200110517    3c                   endif
063300110517
063400110517     c                   add       1             xx
063500110517    2c                   enddo
063600110526     c
063700110526     c* controllo se devo annullare sul traino di copia modificato
063800110526    2c                   if        oldtrns<>vidtrns  and oldtrns>0
063900110526     c                   eval      ktrns=oldtrns
064000110526     c                   eval      kgse=d00gse
064100110526     c                   z-add     1             xx
064200110526    3c                   dow       oldde(xx)>0
064300110526     c                   eval      kdde=oldde(XX)
064400110526
064500110526     c     ktlssc        chain     tntls01l
064600110526     c                   exsr      Anntls
064700110526     c                   add       1             xx
064800110526    3c                   enddo
064900110526     c
065000110526    2c                   endif
065100110526    1c                   endif
065200110517
065300110509     C                   ENDSR
065400000000     C*------------------------------------------------------------------------*
065500000000     C* F10GES - GESTIONE TASTO FUNZIONALE F10 ==> INSERIMENTO
065600000000     C*------------------------------------------------------------------------*
065700000000     C     F10GES        BEGSR
065800000000     C                   ENDSR
065900000000     C*------------------------------------------------------------------------*
066000000000     C* DECODI - CONTROLLA LE SCHIERE DEL GIORNO
066100000000     C*------------------------------------------------------------------------*
066200000000     C     DECODI        BEGSR
066300110509     C                   MOVEL     '0'           $ERR
066400110509     C                   MOVEL     *BLANKS       $MSG
066500110509     c                   setoff                                       606162
066600110509     c                   setoff                                       63
066700110517     c                   clear                   copmod
066800110517     c                   clear                   coptrns
066900110517     c                   clear                   copdde1
067000110517     c                   clear                   copdde2
067100110517     c                   clear                   copdde3
067200110517     c                   clear                   copdsc1
067300110517     c                   clear                   copdsc2
067400110517     c                   clear                   copdsc3
067500110517     c                   clear                   cdde
067600110517     c                   clear                   cdsc
067700110509     C* REIMPOSTA IL FLAG PER EMETTERE IL MESSAGGIO DI AVVERTIMENTO DELL' F12
067800110509     C                   MOVEL     '0'           MSGF12                         *EMETTE MESSAGGIO
067900110509     C*
068000110509IF  0C     $TGES         IFEQ      'G'                                          *SOLO IN GESTIONE
068100110513     c* Traino di scambio
068200130320IF  2C****               IF        vidtrns=*zeros  or
068300130320     C                   if        VIDTRNS=D00TRN
068400110513     C                   MOVEL     '1'           $ERR
068500110513     C                   MOVEL     MSG(7)        $MSG
068600110513     C                   EXSR      CARMSG
068700110513     C                   SETON                                        61
068800110513     C                   LEAVEsr
068900110513E   2C                   ENDIF
069000110513     c*
069100110513     c**                 testn                   vidtrns              30
069200110513     c**                 if        not *in30   or %subst(vidtrns:7:1)<'0'
069300110513     C*+                 MOVEL     '1'           $ERR
069400110513     C**                 MOVEL     MSG(7)        $MSG
069500110513     C**                 EXSR      CARMSG
069600110513     C**                 SETON                                        61
069700110513     C**                 LEAVEsr
069800110513E   2C**                 ENDIF
069900110513     c*
070000110513     c* verifico se reale e decorrente
070100110607     c                   clear                   vidctfp
070200110607     c                   clear                   vidctfa
070300110607     c                   clear                   vidcttr
070400110513     c                   clear                   wok               1
070500110517     c                   z-add     1             xx                3 0
070600130320     c
070700130320     c                   if        vidtrns>0
070800130320     c
070900110513     c     vidtrns       setgt     tntlt01l
071000110513     c     vidtrns       readpe(n) tntlt01l
071100110513     c                   dow       not %eof(tntlt01l)
071200110525     c                   if        tltatb=' '
071300110513     c
071400110513     c                   if        (tltdde<=d00dde and tltdsc>=d00dde) or
071500110513     c                             (tltdde<=savdsc and tltdsc>=savdsc)
071600110513     c                   eval      wok='S'
071700110517     c
071800110607     c                   if        vidctfp=0
071900110607     c* visualizzo le fermate del traino
072000110607     c                   eval      vidctfp=tlttfp
072100110607     c                   eval      vidctfa=tlttfa
072200110607     c                   z-add     1             ktbkut
072300110607     C                   MOVEL     'TV'          KTBCOD
072400110607     C                   MOVEL(P)  TLTTTR        KTBKEY
072500110607     C     KEYTAB        CHAIN     TABEL00F                           99
072600110607     C                   MOVEL     TBLUNI        DSTV
072700110607     C                   MOVEL     §TVDES        VidCTTR                         *TIPO TRAINO
072800110607     c                   endif
072900110607     c
073000110517     c                   eval      coptrns=tlttrn
073100110517     c                   eval      cdde(xx)=tltdde
073200110517     c                   eval      cdsc(xx)=tltdsc
073300110517     c                   add       1             xx
073400110517     c                   endif
073500110525     c                   endif
073600110517     c
073700110513     c     vidtrns       readpe(n) tntlt01l
073800110513     c                   enddo
073900110517     c
074000110513     c                   if        Wok<>'S'
074100110513     C                   MOVEL     '1'           $ERR
074200110513     C                   MOVEL     MSG(11)       $MSG
074300110513     C                   EXSR      CARMSG
074400110513     C                   SETON                                        61
074500110513     C                   LEAVEsr
074600110513E   2C                   ENDIF
074700110513     c
074800110513     c* se sono in immissione e tutti gli altri campi sono vuoti--> li imposto
074900110513     c                   if        *in80 and vidpsc=*blanks and
075000110513     c                             vidggs=' ' and vidhhs=*zeros and
075100110513     c                             vidnot1=*blanks
075200110513     c                   exsr      CArPSC
075300110513     c                   endif
075400110513     c
075500110607     c* se cambiato traino pulisco i flag di forzatura
075600110513     c                   if        vidtrns<>savtrns
075700110513     c                   clear                   forzaSC           1
075800110513     c                   clear                   forzaTRNS         1
075900110513     c                   clear                   forzaTRAL         1
076000110513     c                   eval      savtrns=vidtrns
076100110526     c                   setoff                                       8182
076200110513     c                   endif
076300110513     c
076400110513     c* Verifico se ha lo stesso traino di scambio
076500110513     c                   eval      kgse=d00gse
076600110526     c                   eval      ktrns=vidtrns
076700110517     c                   eval      kdde=cdde(1)
076800110513     c     ktlssc        chain(n)  tntls01l
076900110525    1c                   if        (not%found(tntls01l) or tlsatb<>' ')  and
077000110525     c                             kgse<>*blanks
077100110513     c                   clear                   kgse
077200110513     c     ktlssc        chain(n)  tntls01l
077300110513    1c                   endif
077400110513     c
077500110526    1c**                 if        (not %found(tntls01l) or tlsatb<>' ') and
077600110526     c**                            forzaTRNS=' '
077700110526     C**                 MOVEL     '1'           $ERR
077800110526     C**                 MOVEL     MSG(13)       $MSG
077900110526     C**                 EXSR      CARMSG
078000110526     C**                 SETON                                        61
078100110526     c**                 eval      forzaTRNS='S'
078200110526     C**                 LEAVEsr
078300110526E   1C**                 ENDIF
078400110513     c
078500110525    1c                   if        (not %found(tntls01l) or tlsatb<>' ') and
078600110525    1c                              forzaTRAL=' '
078700110513     c* Verifico se questo traino è stato già indicato in altro traino
078800110513     c                   eval      modsql='C'
078900110513     c                   exsr      SqlTRNS
079000110513    2c                   if        $err='1'
079100110513     c                   eval      forzaTRAL ='S'
079200110513     C                   LEAVEsr
079300110513E   2C                   ENDIF
079400110513E   1C                   ENDIF
079500110513     c
079600110513
079700110525    1c                   if        %found(tntls01l) and tlsatb=' ' and
079800110525    1c                              forzaSC=' '
079900110513    2c                   if        tlstrns<>d00trn
080000110513     C                   MOVEL     '1'           $ERR
080100110513     C                   MOVEL     MSG(14)       $MSG
080200110513     c                   eval      %subst($msg:8:7)=%editc(vidtrns:'Z')
080300110513     c                   eval      %subst($msg:56:7)=%editc(tlstrns:'Z')
080400110513     C                   EXSR      CARMSG
080500110513     C                   SETON                                        61
080600110513     c                   eval      forzaSC ='S'
080700110513     C                   LEAVEsr
080800110513E   2C                   ENDIF
080900110513E   1C                   ENDIF
081000130320     c
081100130320     c                   endif
081200110509
081300110509     c* punto di scambio
081400130320     c                   setoff                                       11
081500110509IF  2C     vidpsc        IFeq      *blanks
081600110509     C                   MOVEL     '1'           $ERR
081700110509     C                   MOVEL     MSG(6)        $MSG
081800110509     C                   EXSR      CARMSG
081900110509     C                   SETON                                        60
082000110509     C                   LEAVEsr
082100110509E   2C                   ENDIF
082200110509
082300110509     c* col '?'  ricerca
082400110509     C                   EVAL      INDX=%scan('?':vidpsc)
082500110509     c
082600110509     c                   if        indx>0
082700110509     c* imposta la ds di procedura
082800110509     c                   CLEAR                   tntlp0ds
082900110509     c                   MOVE      '01'          dp0op0                         *livello procedura
083000110509     c                   MOVEL     *blanks       dp0op1                         *livello programma
083100110509     c                   MOVEL     '0'           dp0f03                         *premuto F03
083200110509     c                   MOVEL     '0'           dp0f12                         *premuto F12
083300110509     c                   MOVEL     '0'           dp0err                         *errore
083400110509     c                   call      'TNTLP0R'
083500110509     c                   parm                    kpjba
083600110509     c                   parm                    TNTLp0ds
083700110509     c
083800110509     c                   eval      vidpsc=dp0psc
083900110512     c                   eval      desloc=dp0loc
084000110513     c                   eval      desnot=dp0not
084100110512     c                   if        dp0prv<>*blanks
084200110513     c                   eval      desloc=%trim(dp0loc)+'  '+dp0prv+dp0nar
084300110512     c                   else
084400110512     c                   eval      desloc=%trim(dp0loc)+' '+dp0nar
084500110512     c                   endif
084600110512     c                   eval      desind=dp0ind
084700110512     c                   eval      descap=dp0cap
084800130320     c* filiale di ritorno se presente
084900130320     c                   eval      dpscflo=dp0flo
085000130320     c                   if        §pscflr>*zeros
085100130320     c                   seton                                        11
085200130320     c                   movel     §pscflr       desflr
085300130320     c                   movel     §pscflr       w0030             3 0
085400130320     c     w0030         chain     azorg01l
085500130320     c                   if        %found(azorg01l)
085600130320     c                   movel     orgdes        desdflr
085700130320     c                   endif
085800130320     c                   endif
085900130320     c
086000130320     c
086100110509     C                   SETON                                        60
086200110509     C                   MOVEL     '1'           $ERR
086300110509     C                   LEAVEsr
086400110509     c                   endif
086500110509     c
086600110509     c     vidpsc        chain     tnpsc01l
086700110509     c                   if        not %found(tnpsc01l)
086800110509     C                   MOVEL     '1'           $ERR
086900110509     C                   MOVEL     MSG(6)        $MSG
087000110509     C                   EXSR      CARMSG
087100110509     C                   SETON                                        60
087200110509     C                   LEAVEsr
087300110509E   2C                   ENDIF
087400110512     c
087500110512     c                   eval      desloc=pscloc
087600110513     c                   eval      desnot=pscnot
087700110512     c                   if        pscprv<>*blanks
087800110513     c                   eval      desloc=%trim(desloc)+'  '+pscprv+pscnar
087900110512     c                   else
088000110512     c                   eval      desloc=%trim(desloc)+' '+pscnar
088100110512     c                   endif
088200110512     c                   eval      desind=pscind
088300110512     c                   eval      descap=psccap
088400130320     c* filiale di ritorno se presente
088500130320     c                   eval      dpscflo=pscflo
088600130320     c
088700130320     c                   if        §pscflr>*zeros
088800130320     c                   seton                                        11
088900130320     c                   movel     §pscflr       desflr
089000130320     c                   movel     §pscflr       w0030             3 0
089100130320     c     w0030         chain     azorg01l
089200130320     c                   if        %found(azorg01l)
089300130320     c                   movel     orgdes        desdflr
089400130320     c                   endif
089500130320     c                   endif
089600110512
089700110509     c* controllo se scaduto
089800110509     c                   if        pscdsc>0
089900110510     c                             and savdsc>pscdsc
090000110509     C                   MOVEL     '1'           $ERR
090100110509     C                   MOVEL     MSG(10)       $MSG
090200110509     C                   EXSR      CARMSG
090300110509     C                   SETON                                        60
090400110509     C                   LEAVEsr
090500110509     c                   endif
090600110509     c
090700110513     c* Giorno
090800110509     C                   EVAL      INDX=%LOOKUP(VIDGGS:ALF)
090900110509     C                   if        Indx=0
091000110509     C                   MOVEL     '1'           $ERR
091100110509     C                   MOVEL     MSG(8)        $MSG
091200110509     C                   EXSR      CARMSG
091300110509     C                   SETON                                        62
091400110509     C                   LEAVEsr
091500110509E   2C                   ENDIF
091600110509     c
091700110509     c* Orario
091800110509     C                   Z-ADD     vidhhs        WORA
091900110509     C                   EXSR      CHKORA
092000110509IF  5C     $ERR          IFEQ      '1'
092100110509     C                   MOVEL     '1'           $ERR
092200110509     C                   MOVEL     MSG(9)        $MSG
092300110509     C                   EXSR      CARMSG
092400110510     C                   SETON                                        63
092500110509     C                   LEAVEsr
092600110509E   2C                   ENDIF
092700110608     c
092800110608     c* Controllo che non sia minore del primo carico e  > dell'ultimo scarico
092900110608     c                   if        vidggs<>savggs  or savhhs<>vidhhs
093000110608     c                   clear                   forzamin          1
093100110608     c                   clear                   forzamax          1
093200110608     c                   eval      savggs=vidggs
093300110608     c                   eval      savhhs=vidhhs
093400110608     c                   endif
093500110608     c
093600110608     c                   if        forzamin=' '
093700110607     c                   if        vidggs<primcag or (vidggs=primcag and
093800110607     c                             vidhhs<=primcah)
093900110607     C                   MOVEL     '1'           $ERR
094000110607     C                   MOVEL     MSG(16)       $MSG
094100110608     c                   eval      forzamin='S'
094200110607     C                   EXSR      CARMSG
094300110607     C                   SETON                                        63
094400110607     C                   LEAVEsr
094500110607     c                   endif
094600110608     c                   endif
094700110608     c                   if        forzamax=' '
094800110607     c                   if        vidggs>ULTSCAg or (vidggs=ultscag and
094900110607     c                             vidhhs>=ultscah)
095000110607     C                   MOVEL     '1'           $ERR
095100110607     C                   MOVEL     MSG(17)       $MSG
095200110608     c                   eval      forzamax='S'
095300110607     C                   EXSR      CARMSG
095400110607     C                   SETON                                        63
095500110607     C                   LEAVEsr
095600110607     c                   endif
095700110608     c                   endif
095800130320     c
095900130320     C                   if        VIDTRNS=0 and §pscflr<=*zeros
096000130320     C                   MOVEL     '1'           $ERR
096100130320     C                   MOVEL     MSG(7)        $MSG
096200130320     C                   EXSR      CARMSG
096300130320     C                   SETON                                        61
096400130320     C                   LEAVEsr
096500130320E   2C                   ENDIF
096600130320     c* se entrambi pieni --> errore
096700130320     C                   if        VIDTRNS>0 and §pscflr> *zeros
096800130320     C                   MOVEL     '1'           $ERR
096900130320     C                   MOVEL     MSG(18)       $MSG
097000130320     C                   EXSR      CARMSG
097100130320     C                   SETON                                        60
097200130320     C                   LEAVEsr
097300130320E   2C                   ENDIF
097400110607     c
097500110509E   2C                   ENDIF
097600110509     C*
097700000000     C                   ENDSR
097800110513      *-----------------------------------------------------***********
097900110513      * con SQL controllo decorrenza / scadenza record
098000110513      *-----------------------------------------------------***********
098100110513     c     SQLTRNS       BEGSR
098200110513     c                   clear                   wok
098300110513     c                   eval      wrkgetlista =
098400110513     c                             'select *       +
098500110513     c                               from tnTLS00f where tlstrns=' +
098600110513     c                               %editc(d00trn:'X') + ' and tlsatb='' '''
098700110513     c
098800110513     C/EXEC SQL
098900110513     C+ PREPARE s1 FROM :wrkgetlista
099000110513     C/END-EXEC
099100110513     C
099200110513     C/EXEC SQL
099300110513     C+ DECLARE A1 CURSOR FOR S1
099400110513     C/END-EXEC
099500110513     C
099600110513     C/EXEC SQL
099700110513     C+ OPEN a1
099800110513     C/END-EXEC
099900110513
100000110513      *?Leggo il file
100100110513    1c                   do        *hival
100200110513     C/EXEC SQL
100300110513     C+ FETCH NEXT FROM a1 INTO : tntlsds
100400110513     C/END-EXEC
100500110513
100600110513    2c                   select
100700110513
100800110513     c                   when      sqlcod = 100
100900110513     c* non ne ho trovato nemmeno uno--> ok
101000110513     c
101100110513     c                   leave
101200110513     c                   when      sqlcod < 0
101300110513     c                   seton                                        H1
101400110513     c                   leave
101500110513
101600110513     c                   other
101700110513     c
101800110513     c* Deve avere lo stesso giorno
101900110513    3c                   if        tlsgse=d00gse
102000110513     c     ktlT2         chain     tntlt01l
102100110513     c*  verifico la decorrenza
102200110525    4c                   if        %found(tntlt01l) and
102300110525     c                             ((tlTdde<=d00dde and tlTdsc>=d00dde) or
102400110525     c                             (tltdde<=savdsc and tltdsc>=savdsc))
102500110513     c                   eval      wok='N'
102600110513     c                   leave
102700110513    4c                   endif
102800110513    3c                   endif
102900110513     c
103000110513    2c                   endsl
103100110513
103200110513    1c                   enddo
103300110513     c* errore
103400110513    0c                   if        wok='N'
103500110513     c* modalità Controllo  --> errore
103600110513    1c                   if        modsql='C'
103700110513     C                   MOVEL     '1'           $ERR
103800110513     C                   MOVEL     MSG(15)       $MSG
103900110513     c                   eval      %subst($msg:8:7)=%editc(tlstrn:'Z')
104000110513     c                   eval      %subst($msg:57:7)=%editc(d00trn:'Z')
104100110513     C                   EXSR      CARMSG
104200110513     C                   SETON                                        61
104300110513     C                   LEAVEsr
104400110513    1c                   endif
104500110513    0c                   endif
104600110513
104700110513     C/EXEC SQL
104800110513     C+ CLOSE a1
104900110513     C/END-EXEC
105000110513
105100110513     c                   ENDSR
105200110526     C*------------------------------------------------------------------------*
105300110526     C* dati did TNTLs in videata
105400110526     C*------------------------------------------------------------------------*
105500110526     c     DatiTLS       BEGSR
105600110526     c                   eval      vidpsc=tlspsc
105700110526     c     vidpsc        chain     tnpsc01l
105800110526    2c                   if        %found(tnpsc01l)
105900110526     c                   eval      desloc=pscloc
106000110526     c                   eval      desnot=pscnot
106100110526    3c                   if        pscprv<>*blanks
106200110526     c                   eval      desloc=%trim(desloc)+'  '+pscprv+pscnar
106300110526   x3c                   else
106400110526     c                   eval      desloc=%trim(desloc)+' '+pscnar
106500110526    3c                   endif
106600110526     c                   eval      desind=pscind
106700110526     c                   eval      descap=psccap
106800130320     c* filiale di ritorno se presente
106900130320     c                   eval      dpscflo=pscflo
107000130320     c
107100130320     c                   if        §pscflr>*zeros
107200130320     c                   seton                                        11
107300130320     c                   movel     §pscflr       desflr
107400130320     c                   movel     §pscflr       w0030             3 0
107500130320     c     w0030         chain     azorg01l
107600130320     c                   if        %found(azorg01l)
107700130320     c                   movel     orgdes        desdflr
107800130320     c                   endif
107900130320     c                   endif
108000110526    2c                   endif
108100110526
108200110526     c                   eval      vidggs=tlsggs
108300110526     c                   eval      vidhhs=tlshhs
108400110526     c                   movel     tlsnot        vidnot1
108500110526     c                   eval      vidnot2=%subst(tlsnot:26:25)
108600110526     c                   eval      vidnot3=%subst(tlsnot:51:10)
108700110526     c                   ENDSR
108800110513      *-----------------------------------------------------***********
108900110513      * carico dati di scambio in inserimento
109000110513      *-----------------------------------------------------***********
109100110513     c     CARPSC        BEGSR
109200110513     c     ktls2         chain     tntls01l
109300110513     c                   if        %found(tntls01l) and tlsatb=' '
109400110526     c                   exsr      datiTLS
109500110513     c                   endif
109600110513     c
109700110513     c                   ENDSR
109800000000     C*------------------------------------------------------------------------*
109900000000     C* CHKORA - CONTROLLA L'ORA
110000000000     C*------------------------------------------------------------------------*
110100000000     C     CHKORA        BEGSR
110200000000     C*
110300110509     C     *LIKE         DEFINE    VidHHs        WORA
110400000000     C                   Z-ADD     *ZEROS        WHH               2 0
110500000000     C                   Z-ADD     *ZEROS        WMM               2 0
110600000000     C* ORE E MINUTI
110700000000IF  1C     WORA          IFEQ      *ZEROS
110800000000     C                   MOVEL     '1'           $ERR
110900000000E   1C                   ENDIF
111000000000     C* ORE
111100000000     C                   MOVEL     WORA          WHH
111200000000IF  1C     WHH           IFGE      0                                            0 -> 24
111300000000     C     WHH           ANDLE     24
111400000000     C                   ELSE
111500000000     C                   MOVEL     '1'           $ERR
111600000000E   1C                   ENDIF
111700000000     C* MINUTI
111800000000     C                   MOVE      WORA          WMM
111900000000IF  1C     WMM           IFGE      0                                            0 -> 59
112000000000     C     WMM           ANDLE     59
112100000000     C                   ELSE
112200000000     C                   MOVEL     '1'           $ERR
112300000000E   1C                   ENDIF
112400000000     C* ORE 24:00
112500000000IF  1C     WHH           IFEQ      24                                           *SOLO 24:00
112600000000     C     WMM           ANDGT     0
112700000000     C                   MOVEL     '1'           $ERR
112800000000E   1C                   ENDIF
112900000000     C*
113000000000     C                   ENDSR
113100000000     C*------------------------------------------------------------------------*
113200000000     C* LANPGM - LANCIA UN PROGRAMMA
113300000000     C*------------------------------------------------------------------------*
113400000000     C     LANPGM        BEGSR
113500000000     C***
113600000000     C* PARAMETRI INFORMATIVI
113700000000     C***
113800000000     C                   MOVEL     WOP0          D00OP0                         *LIVELLO PROCEDURA
113900000000     C                   MOVEL     WOP1          D00OP1                         *LIVELLO PROGRAMMA
114000000000     C                   MOVEL     '0'           D00F03                         *F03-FINE PGM
114100000000     C                   MOVEL     '0'           D00F12                         *F12-RITORNO
114200000000     C                   MOVEL     '0'           D00ERR                         *ERRORE
114300951212     C                   MOVEL     WMSG          D00MSG                         *MESSAGGIO ERRORE
114400000000     C***
114500000000     C* PARAMETRI
114600000000     C***
114700000000     C                   Z-ADD     WTRN          D00TRN                         *TRAINO
114800000000     C                   Z-ADD     WDDE          D00DDE                         *DECORRENZA (AA/M/G)
114900000000     C                   Z-ADD     WSEQ          D00SEQ                         *SEQUENZA
115000000000     C                   MOVEL     WTFM          D00TFM                         *TIPO FERMATA
115100000000     C                   Z-ADD     WFIL          D00FIL                         *FILIALE FERMATA
115200000000     C                   MOVEL     WGGA          D00GGA                         *GIORNO ARRIVO
115300000000     C                   Z-ADD     WHHA          D00HHA                         *ORA ARRIVO
115400000000     C                   MOVEL     WSCA          D00SCA                         *SCARICO IN ARRIVO
115500000000     C                   MOVEL     WGGP          D00GGP                         *GIORNO PARTENZA
115600000000     C                   Z-ADD     WHHP          D00HHP                         *ORA PARTENZA
115700000000     C                   MOVEL     WCAR          D00CAR                         *CARICO IN PARTENZA
115800000000     C                   Z-ADD     WKMT          D00KMT                         *CHILOMETRI FERMATA
115900000000     C                   Z-ADD     WTEM          D00TEM                         *TEMPO FERMATA
116000000000     C                   MOVEL     WGFV          D00GFV                         *GIORNO F.V.
116100000000     C*--------------------
116200000000     C* L A N C I O
116300000000     C*--------------------
116400000000     C                   MOVEL     DS00          KPJBU
116500000000     C                   CALL      WPGM                                 99
116600000000     C                   PARM                    KPJBA
116700000000     C                   MOVEL     KPJBU         DS00
116800000000     C*
116900000000IF  1C     D00F03        IFEQ      '1'
117000000000     C                   MOVEL     '1'           $F03
117100000000     C                   MOVEL     '1'           $FINE
117200000000E   1C                   ENDIF
117300000000IF  1C     D00F12        IFEQ      '1'
117400000000     C                   MOVEL     '1'           $F12
117500000000E   1C                   ENDIF
117600000000     C                   MOVEL     D00MSG        $MSG
117700000000     C                   EXSR      CARMSG
117800000000     C*
117900000000     C                   ENDSR
118000000000     C*------------------------------------------------------------------------*
118100000000     C* IMPVID - IMPOSTAZIONI VIDEO
118200000000     C*------------------------------------------------------------------------*
118300000000     C     IMPVID        BEGSR
118400110512SL  1C                   SELECT
118500110512     C     D00OP0        WHENEQ    'T02'
118600110512     C                   SETOFF                                       90        *NESSUNA PROTEZIONE
118700110512     C     D00OP0        WHENEQ    'T05'
118800110512     C                   SETON                                        90        *PROTEZIONE COMPLETA
118900110512     C                   OTHER
119000110512E   1C                   ENDSL
119100000000     C*
119200000000     C* CARICA L'ALFABETO
119300000000     C                   MOVEA     DALF          ALF
119400000000     C* DATI TRAINO
119500000000     C                   Z-ADD     D00TRN        VCCTRN                         *TRAINO
119600000000     C                   Z-ADD     D00DDE        G08INV
119700000000     C                   Z-ADD     *ZEROS        G08DAT
119800000000     C                   MOVEL     '3'           G08ERR
119900000000     C                   CALL      'XSRDA8'
120000000000     C                   PARM                    WLBDA8
120100000000     C                   Z-ADD     G08DAT        VCCDDE                         *DATA DECORRENZA
120200000000     C* LEGGE IL TRAINO
120300000000     C                   Z-ADD     D00TRN        KLTTRN
120400000000     C                   Z-ADD     D00DDE        KLTDDE
120500000000     C     KEYTLT        CHAIN(N)  TNTLT01L                           99
120600020715     c                   z-add     1             ktbkut
120700000000     C                   MOVEL     'TV'          KTBCOD
120800000000     C                   MOVEL(P)  TLTTTR        KTBKEY
120900000000     C     KEYTAB        CHAIN     TABEL00F                           99
121000000000     C                   MOVEL     TBLUNI        DSTV
121100000000     C                   MOVEL     §TVDES        VCCTTR                         *TIPO TRAINO
121200000000     C                   MOVEL     §TVDEF        DEPDEF                         *TRAINO DEFLUENZA
121300090115     C                   MOVEL     §TVutc        DEPutc
121400110510     C                   Z-ADD     TLTDSC        savdsc            8 0
121500110510     C                   Z-ADD     TLTDSC        G08INV
121600000000     C                   Z-ADD     *ZEROS        G08DAT
121700000000     C                   MOVEL     '3'           G08ERR
121800000000     C                   CALL      'XSRDA8'
121900000000     C                   PARM                    WLBDA8
122000000000     C                   Z-ADD     G08DAT        VCCDSC                         *DATA SCADENEZA
122100000000     C                   Z-ADD     TLTTFP        VCCTFP                         *TERMINAL PARTENZA
122200000000     C                   Z-ADD     TLTTFA        VCCTFA                         *TERMINAL PARTENZA
122300000000     C                   MOVEA     TLTGSE        GIO                            *PARTENZE
122400000000     C                   MOVEL     GIO(1)        VCCGS1
122500000000     C                   MOVEL     GIO(2)        VCCGS2
122600000000     C                   MOVEL     GIO(3)        VCCGS3
122700000000     C                   MOVEL     GIO(4)        VCCGS4
122800000000     C                   MOVEL     GIO(5)        VCCGS5
122900000000     C                   MOVEL     GIO(6)        VCCGS6
123000000000     C                   MOVEL     GIO(7)        VCCGS7
123100110512     c
123200110512     c* Se traino in annullamento --> proteggo
123300110512     c                   if        tltatb<>*blanks
123400110512     c                   seton                                        90
123500110512     C                   MOVEL     DESATB        vccnew                         *RECORD ANNULLATO
123600110512     C                   MOVEL     '1'           *IN47                          *VISUALIZZATO
123700110512     c                   endif
123800960423     C*
123900960423     C* SE TIPO TRAINO INOLTRO METTE NELLA DESCRIZIONE IL TIPO TRAINO ASSOCIATO
124000960423IF  1C     TLTTTR        IFEQ      'I'
124100960423     C                   CLEAR                   SA7
124200960423     C                   MOVEA     'INL'         SA7
124300960423     C                   MOVEL     '('           SA7(5)
124400960423     C                   MOVEL     D00INL        SA7(6)
124500960423     C                   MOVEL     ')'           SA7(7)
124600960423     C                   MOVEA     SA7           VCCTTR
124700960423E   1C                   ENDIF
124800000000     C*
124900110506     c* Carico i dati del punti di scambio se presenti altrimenti videata vuota
125000110526     c                   clear                   oldde
125100110509     c     keytls        chain     tntls01l
125200110513     c
125300110525     c                   if        %found(tntls01l)  and tlsatb=' '
125400110526     c                   exsr      DatiTLS
125500110512
125600110526     c                   eval      OLDtrns=tlstrns
125700110526     c                   eval      SAVtrns=tlstrns
125800110513     c                   eval      vidtrns=tlstrns
125900110608     c                   eval      savggs =tlsggs
126000110608     c                   eval      savhhs =tlshhs
126100110526
126200110513     c* chaino per avere i dati del traino di copia
126300110607     c                   clear                   vidctfp
126400110607     c                   clear                   vidctfa
126500110526     c                   z-add     1             xx
126600110526     c     vidtrns       setgt     tntlt01l
126700110526     c     vidtrns       readpe(n) tntlt01l
126800110510     c                   dow       not %eof(tntlt01l)
126900110526     c                   if        tltatb=' '
127000110526     c                   if        (tltdde<=d00dde and tltdsc>=d00dde) or
127100110526     c                             (tltdde<=savdsc and tltdsc>=savdsc)
127200110526     c                   eval      wok='S'
127300110607     c
127400110607     c                   if        vidctfp=0
127500110607     c                   eval      vidctfp=tlttfp
127600110607     c                   eval      vidctfa=tlttfa
127700110607     c                   z-add     1             ktbkut
127800110607     C                   MOVEL     'TV'          KTBCOD
127900110607     C                   MOVEL(P)  TLTTTR        KTBKEY
128000110607     C     KEYTAB        CHAIN     TABEL00F                           99
128100110607     C                   MOVEL     TBLUNI        DSTV
128200110607     C                   MOVEL     §TVDES        VidCTTR                         *TIPO TRAINO
128300110607     c                   endif
128400110526     c
128500110526     c                   eval      oldde(xx)=tltdde
128600110526     c                   add       1             xx
128700110526     c                   endif
128800110526     c                   endif
128900110526     c
129000110526     c     vidtrns       readpe(n) tntlt01l
129100110526     c                   enddo
129200110510     c
129300110513     c                   setoff                                       80
129400110510     c
129500110509     c                   else
129600110608     c                   clear                   savggs
129700110608     c                   clear                   savhhs
129800110608     c                   clear                   vidpsc
129900110512     c                   clear                   desloc
130000110513     c                   clear                   desnot
130100110512     c                   clear                   desind
130200110512     c                   clear                   descap
130300110513     c                   clear                   SAVtrns
130400110526     c                   clear                   OLDtrns
130500110513     c                   clear                   vidtrns
130600110510     c                   clear                   vidctfp
130700110510     c                   clear                   vidctfa
130800110510     c                   clear                   vidcttr
130900110509     c                   clear                   vidggs
131000110509     c                   clear                   vidhhs
131100110509     c                   clear                   vidnot1
131200110513     c                   clear                   vidnot2
131300110513     c                   clear                   vidnot3
131400130320     c                   clear                   desflr
131500130320     c                   clear                   desdflr
131600110513     c                   seton                                        80
131700110509     c                   endif
131800110506     c
131900110513     c                   clear                   forzaSC
132000110608     c                   clear                   forzaMIN
132100110608     c                   clear                   forzaMAx
132200110513     c                   clear                   forzaTRNS
132300110513     c                   clear                   forzaTRAL
132400110517     c                   clear                   coptrns
132500110517     c                   clear                   copdde1
132600110517     c                   clear                   copdde2
132700110517     c                   clear                   copdde3
132800110517     c                   clear                   copdsc1
132900110517     c                   clear                   copdsc2
133000110517     c                   clear                   copdsc3
133100110517     c                   eval      copcop='SI'
133200110517     c                   clear                   copmod
133300110526     c                   clear                   copmod2
133400110526     c                   clear                   copoldtrns
133500110526     c                   setoff                                       8182
133600110506     C                   ENDSR
133700000000     C*------------------------------------------------------------------------*
133800000000     C* CARTAB - CARICA LE TABELLE
133900000000     C*------------------------------------------------------------------------*
134000000000     C     CARTAB        BEGSR
134100000000     C                   ENDSR
134200971020     C*------------------------------------------------------------------------*
134300971020     C* FINSR - OPERAZIONI FINALI
134400971020     C*------------------------------------------------------------------------*
134500971020     C     FINSR         BEGSR
134600971020     C* FNLV55R
134700971020     C                   CLEAR                   DSLV55                         *RECUPERA TERMINALs
134800971020     C                   MOVEL     'C'           D55TLA
134900971020     C                   CALL      'FNLV55R'
135000971020     C                   PARM                    DSLV55
135100971020     C*
135200971020     C                   ENDSR
135300000000     C*------------------------------------------------------------------------*
135400000000     C* *INZSR - OPERAZIONI INIZIALI
135500000000     C*------------------------------------------------------------------------*
135600000000     C     *INZSR        BEGSR
135700000000     C*--------------------
135800000000     C* RICEVIMENTO PARAMETRI
135900000000     C*--------------------
136000000000     C     *ENTRY        PLIST
136100000000     C                   PARM                    KPJBA
136200000000     C                   MOVEL     KPJBU         DS00
136300000000     C*--------------------
136400000000     C* VARIABILI RIFERITE AL DATA BASE
136500000000     C*--------------------
136600000000     C     *LIKE         DEFINE    TBLKUT        KTBKUT                         *TABEL00F
136700000000     C     *LIKE         DEFINE    TBLCOD        KTBCOD
136800000000     C     *LIKE         DEFINE    TBLKEY        KTBKEY
136900000000     C     *LIKE         DEFINE    TLTTRN        KLTTRN                         *TNTLT00F
137000000000     C     *LIKE         DEFINE    TLTDDE        KLTDDE
137100110509     C     *LIKE         DEFINE    TLsgse        KLsgse
137200000000     C     *LIKE         DEFINE    D00OP0        WOP0                           *DS00
137300000000     C     *LIKE         DEFINE    D00OP1        WOP1
137400000000     C     *LIKE         DEFINE    D00F03        WF03
137500000000     C     *LIKE         DEFINE    D00F12        WF12
137600000000     C     *LIKE         DEFINE    D00ERR        WERR
137700000000     C     *LIKE         DEFINE    D00MSG        WMSG
137800000000     C     *LIKE         DEFINE    D00TRN        WTRN
137900000000     C     *LIKE         DEFINE    D00DDE        WDDE
138000000000     C     *LIKE         DEFINE    D00SEQ        WSEQ
138100000000     C     *LIKE         DEFINE    D00TFM        WTFM
138200000000     C     *LIKE         DEFINE    D00FIL        WFIL
138300000000     C     *LIKE         DEFINE    D00GGA        WGGA
138400000000     C     *LIKE         DEFINE    D00HHA        WHHA
138500000000     C     *LIKE         DEFINE    D00SCA        WSCA
138600000000     C     *LIKE         DEFINE    D00GGP        WGGP
138700000000     C     *LIKE         DEFINE    D00HHP        WHHP
138800000000     C     *LIKE         DEFINE    D00CAR        WCAR
138900000000     C     *LIKE         DEFINE    D00KMT        WKMT
139000000000     C     *LIKE         DEFINE    D00TEM        WTEM
139100000000     C     *LIKE         DEFINE    D00GFV        WGFV
139200000000     C     *LIKE         DEFINE    D00SEQ        PSEQ                           *DI PARK
139300000000     C     *LIKE         DEFINE    D00TFM        PTFM
139400000000     C     *LIKE         DEFINE    D00FIL        PFIL
139500000000     C     *LIKE         DEFINE    D00GGA        PGGA
139600000000     C     *LIKE         DEFINE    D00HHA        PHHA
139700000000     C     *LIKE         DEFINE    D00SCA        PSCA
139800000000     C     *LIKE         DEFINE    D00GGP        PGGP
139900000000     C     *LIKE         DEFINE    D00HHP        PHHP
140000000000     C     *LIKE         DEFINE    D00CAR        PCAR
140100000000     C     *LIKE         DEFINE    D00KMT        PKMT
140200000000     C     *LIKE         DEFINE    D00TEM        PTEM
140300000000     C     *LIKE         DEFINE    D00GFV        PGFV
140400000000     C     *LIKE         DEFINE    D00SEQ        DEPSEQ                         *DEPOSITO
140500971128     C     *LIKE         DEFINE    ORGFIL        DEPFEL                         *DEPOSITI
140600971128     C     *LIKE         DEFINE    ORGFIL        DEPTFP
140700971128     C     *LIKE         DEFINE    ORGFIL        DEPTFA
140800000000     C     *LIKE         DEFINE    §TVDEF        DEPDEF
140900090115     C     *LIKE         DEFINE    §TVutc        DEPutc
141000000000     C*--------------------
141100000000     C* CHIAVI DI LETTURA
141200000000     C*--------------------
141300000000     C* LETTURA TABEL00F - COMPLETA
141400000000     C     KEYTAB        KLIST
141500000000     C                   KFLD                    KTBKUT                         *CODICE UTENTE
141600000000     C                   KFLD                    KTBCOD                         *TABELLA
141700000000     C                   KFLD                    KTBKEY                         *CHIAVE TABELLA
141800110509     C* LETTURA TNTLT01L - COMPLETA
141900110509     C     KEYTLT        KLIST
142000110509     C                   KFLD                    KLTTRN                         *NUMERO TRAINO
142100110509     C                   KFLD                    KLTDDE                         *DATA DECORRENZA
142200110513     C* LETTURA TNTLs01L -
142300110506     C     KEYTLs        KLIST
142400110506     C                   KFLD                    D00TRN                         *TRAINO
142500110506     C                   KFLD                    D00DDE                         *DATA DECORRENZA
142600110506     C                   KFLD                    D00GSE                         *DATA DECORRENZA
142700110607     C* LETTURA TNTLOO1L -
142800110607     C     KEYTLO        KLIST
142900110607     C                   KFLD                    D00TRN                         *TRAINO
143000110607     C                   KFLD                    D00DDE                         *DATA DECORRENZA
143100110607     C                   KFLD                    KGSE
143200110513     C* LETTURA TNTLs01L - per controllo traino di scambio "reciproco"
143300110513     C     KTLSSC        KLIST
143400110526     C                   KFLD                    ktrns                          *TRAINO
143500110517     C                   KFLD                    kdde                           *DATA DECORRENZA
143600110513     C                   KFLD                    kgse                           *GG apertura
143700110513     C     KTLT2         KLIST
143800110513     C                   KFLD                    TLSTRN                         *TRAINO
143900110513     C                   KFLD                    TLSDDE                         *DATA DECORRENZA
144000110513     C     KTLS2         KLIST
144100110513     C                   KFLD                    TLtTRN                         *TRAINO
144200110513     C                   KFLD                    TLtDDE                         *DATA DECORRENZA
144300110513     C                   KFLD                    d00gse                         *DATA DECORRENZA
144400000000     C*--------------------
144500000000     C* VARIABILI DI CONTROLLO
144600000000     C*--------------------
144700000000     C                   MOVEL     'N'           WRECOK            1            *VALIDITA' RECORD
144800000000     C                   MOVEL     '1'           MSGF12            1            *MESSAGGIO PER F12
144900971020     C*
145000971020     C* CALCOLA LA DATA CORRENTE
145100971020     C                   TIME                    N14              14 0          *ORA (6)+ DATA (8)
145200971020     C                   MOVE      N14           N8                8 0          *DATA (8) IN G/M/AA
145300971020     C                   Z-ADD     N8            G08DAT
145400971020     C                   Z-ADD     *ZEROS        G08INV
145500971020     C                   MOVEL     '0'           G08ERR
145600971020     C                   CALL      'XSRDA8'
145700971020     C                   PARM                    WLBDA8
145800971020     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
145900000000     C***
146000000000     C* IMPOSTAZIONI VIDEO
146100000000     C***
146200000000     C* 02 -> GESTIONE / 05 -> INTERROGAZIONE
146300000000     C                   MOVE      D00OP0        INTGES            2
146400000000IF  1C     INTGES        IFEQ      '02'
146500000000     C                   MOVEL     'G'           $TGES             1            *G=GESTIONE
146600000000     C                   MOVEL     DESGES        VTDOPE                         *DESCR OPERAZIONE
146700000000X   1C                   ELSE
146800000000     C                   MOVEL     'I'           $TGES             1            *I=INTERROGAZIONE
146900000000     C                   MOVEL     DESINT        VTDOPE                         *DESCR OPERAZIONE
147000000000E   1C                   ENDIF
147100000000     C*
147200000000     C* I FILES SONO STATI APERTI IN I/O PERCHE' COSI' VUOLE OVRDBF SHARE(*YES);
147300000000     C* SONO PERTANTO NECESSARIE DELLE VARIAZIONI/AGGIUNTE FITTIZIE AI FILES
147400000000IF  1C     *IN99         IFEQ      *ON
147500000000     C     *IN99         ANDEQ     *OFF
147600000000     C* TRAINI
147700000000     C                   UPDATE    TNTLT000
147800000000     C                   WRITE     TNTLT000
147900000000E   1C                   ENDIF
148000110607     C*
148100110607     c* Cerco gli orari della prima fermata e dell'ultima del traino
148200110607     c                   eval      kgse=d00gse
148300110607     c     keytlo        setll     tntlo02l
148400110607     c     keytlo        reade     tntlo02l
148500110607     c* se non trovato giorno specifico leggo giorno a *blanks
148600110607     c                   if        %eof(tntlo02l)
148700110607     c                   eval      kgse=' '
148800110607     c     keytlo        setll     tntlo02l
148900110607     c     keytlo        reade     tntlo02l
149000110607     c                   endif
149100110607     c
149200110607     c                   dow       not %eof(tntlo02l)
149300110607     c* Primo carico
149400110607     c                   if        primcag=' ' and tloggp<>' '
149500110607     c                   eval      primcag=tloggp
149600110607     c                   eval      primcah=tlohhp
149700110607     c                   endif
149800110607     c* Ultimo scarico
149900110607     c                   if        tlogga<>' ' and  (tlogga>ultscag or
150000110607     c                             tlogga=ultscag and tlohha>ultscah)
150100110607     c                   eval      ultscag=tlogga
150200110607     c                   eval      ultscah=tlohha
150300110607     c                   endif
150400110607     c
150500110607     c     keytlo        reade     tntlo02l
150600110607     c                   enddo
150700000000     C*
150800000000     C* ESEGUE OPERAZIONI INIZIALI STANDARD
150900000000     C                   EXSR      INZSTD
151000000000     C*
151100000000     C* IMPOSTAZIONE INIZIALE VIDEO
151200000000     C                   EXSR      IMPVID
151300000000     C*
151400000000     C* CARICA LE TABELLE
151500000000     C                   EXSR      CARTAB
151600000000     C*
151700110506     C* EMETTO VIDEATA
151800110509     C                   EXSR      carv1                                        CARICA E EMETTE SFIL
151900000000     C*
152000000000     C                   ENDSR
152100000000     C*------------------------------------------------------------------------*
152200000000     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
152300000000     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
152400000000     C*    LA PARTE DI SPECIFICHE 'C' SOTTOSTANTE SI RIFA' A PARTICOLARI
152500000000     C*    STANDARDS DI PROGRAMMAZIONE E NELLA MAGGIOR PARTE DEI CASI
152600000000     C*    NON E' NECESSARIO MODIFICARLA, NEL CASO FARE ATTENZIONE.
152700000000     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
152800000000     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
152900000000     C*------------------------------------------------------------------------*
153000000000     C* INZSTD - ROUTINE INIZIALE STANDARD
153100000000     C*------------------------------------------------------------------------*
153200000000     C     INZSTD        BEGSR
153300000000     C* CHIAMA ROUTINE PER RIEMPIRE LA DS DSKEY (TASTI PREMUTI)
153400000000     C                   CALL      'XKEYS'
153500000000     C                   PARM                    DSKEYS
153600000000     C                   MOVEA     KEYRIG        KEYS                           *CARICA SCHIERA FUNZ
153700000000     C* PARAMETRI PER RICHIAMO PGM (ROUTINE) XMSG (MESSAGGI)
153800000000     C     XMSGPA        PLIST
153900000000     C                   PARM                    DSMSMS
154000000000     C                   PARM                    DSMSPG
154100000000     C                   PARM                    DSMSKE
154200000000     C                   PARM                    DSMSER
154300000000     C* IMPOSTA PER CL=XMSG (MESSAGGI ALLA CODA PGM), FISSI IN TUTTO IL PGM
154400110506     C                   MOVEL     'TNTLZ4R'     DSMSPG                         *NOME PGM CHIAMANTE
154500110506     C                   MOVEL     'TNTLZ4R'     VMCQUE                         *CODA MESSAGGI DESTI
154600000000     C                   MOVEL     *BLANKS       VMCKEY                         *CHIAVE MESSAGGIO
154700000000     C                   MOVEL     *BLANKS       DSMSMS                         *MESSAGGIO
154800000000     C* INIZIALIZZO FLAG CONTROLLO PGM
154900000000     C                   MOVEL     '0'           $ERR              1            *ERRORE
155000000000     C                   MOVEL     '0'           $F03              1            *PREMUTO TASTO F03
155100000000     C                   MOVEL     '0'           $F12              1            *PREMUTO TASTO F12
155200000000     C                   MOVEL     *BLANKS       $GES              2            *GESTIONE VIDEO
155300000000     C                   MOVEL     *BLANKS       $MSG             80            *MESSAGGI
155400000000     C                   MOVEL     '0'           $FINE             1            *FINE PGM
155500000000     C                   MOVEL     '0'           $TESVD            1            *1=RIEMETT.TEST.VIDE
155600971128     C*
155700971128     C* REPERIMENTO DATI UTENTE
155800020715
155900020715     c     *dtaara       define    §azute        azuteds
156000020715     c     *dtaara       define    §datiute      ddatiute
156100020715
156200020715     c                   in(E)     *dtaara
156300020715     c                   if        %Error or rsut = *blanks
156400020715     c                   clear                   tibs34ds
156500020715     c                   call      'TIBS34R'
156600020715     c                   parm                    Tibs34Ds
156700020715     c                   in        *dtaara
156800020715     c                   endif
156900000000     C* POSIZIONE MINIMA E MASSIMA DEL CURSORE PER GESTIONE MESSAGGI
157000000000     C                   BITOFF    '01234567'    POSMI1            1
157100000000     C                   BITOFF    '01234567'    POSMI2            1
157200000000     C                   BITOFF    '01234567'    POSMA1            1
157300000000     C                   BITOFF    '01234567'    POSMA2            1
157400000000     C                   BITON     '34'          POSMI1            1            *18
157500000000     C                   BITON     '7'           POSMI2            1            *01
157600000000     C                   BITON     '34'          POSMA1            1            *18
157700000000     C                   BITON     '0'           POSMA2            1            *80
157800000000     C                   MOVE      POSMI2        POSMIN            2
157900000000     C                   MOVEL     POSMI1        POSMIN
158000000000     C                   MOVE      POSMA2        POSMAX            2
158100000000     C                   MOVEL     POSMA1        POSMAX
158200000000     C                   ENDSR
158300000000     C*------------------------------------------------------------------------*
158400000000     C* FUNSTD - LANCIA SUBROUTINE STANDARD
158500000000     C*------------------------------------------------------------------------*
158600000000     C     FUNSTD        BEGSR
158700000000     C                   SELECT
158800000000     C     KEY           WHENEQ    F3
158900000000     C                   EXSR      F03STD
159000000000     C     KEY           WHENEQ    F12
159100000000     C                   EXSR      F12STD
159200000000     C     KEY           WHENEQ    F24
159300000000     C                   EXSR      F24STD
159400000000     C                   OTHER
159500000000     C                   MOVEL     MSG(2)        $MSG
159600000000     C                   EXSR      CARMSG
159700000000     C                   ENDSL
159800000000     C                   ENDSR
159900000000     C*------------------------------------------------------------------------*
160000000000     C* F03STD -GEST.TASTO FUNZ. F03 STANDARD ===> FINE
160100000000     C*------------------------------------------------------------------------*
160200000000     C     F03STD        BEGSR
160300000000     C* IMPOSTO FLAG DI PREMUTO TASTO F03 (A LIVELLO PROCEDURA)
160400000000     C                   MOVEL     '1'           $F03
160500000000     C* IMPOSTO FLAG DI FINE PROGRAMMA
160600000000     C                   MOVEL     '1'           $FINE
160700951212     C* PULISCE IL MESSAGGIO
160800951212     C                   MOVEL     *BLANKS       D00MSG
160900000000     C                   ENDSR
161000000000     C*------------------------------------------------------------------------*
161100000000     C* F12STD -GEST.TASTO FUNZ. F12 STANDARD ===> RITORNO
161200000000     C*------------------------------------------------------------------------*
161300000000     C     F12STD        BEGSR
161400000000     C*
161500000000     C* SOLO LA PRIMA VOLTA MANDA UN MESSAGGIO DI CONTROLLO PER ESSERE SICURI DI
161600000000     C* VOLER TORNARE INDIETRO E NON REGISTRARE LE VARIAZIONI ESEGUITE FIN LI'
161700000000IF  1C     $TGES         IFEQ      'G'                                          *GESTIONE
161800000000     C     MSGF12        ANDEQ     '0'                                          *EMETTERE MESSAGGIO
161900130320     C                   MOVEL     MSG(20)       $MSG
162000000000     C                   EXSR      CARMSG
162100000000     C                   MOVEL     '1'           MSGF12                         *MESSAGGIO EMESSO
162200000000X   1C                   ELSE
162300000000     C*
162400000000     C* IMPOSTO FLAG DI PREMUTO TASTO F12 (A LIVELLO PROCEDURA)
162500000000     C                   MOVEL     '1'           $F12
162600000000     C* IMPOSTO FLAG DI FIN PGM
162700000000     C                   MOVEL     '1'           $FINE
162800951212     C* PULISCE IL MESSAGGIO
162900951212     C                   MOVEL     *BLANKS       D00MSG
163000000000E   1C                   ENDIF
163100000000     C*
163200000000     C                   ENDSR
163300110509     C*------------------------------------------------------------------------*
163400110509     C* F24STD -GEST.TASTO FUNZ. F24 STANDARD ===> ALTRI TASTI
163500110509     C*------------------------------------------------------------------------*
163600110509     C     F24STD        BEGSR
163700110509     C* GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI
163800110509     C                   EXSR      OUTFUN
163900110509     C                   ENDSR
164000000000     C*------------------------------------------------------------------------*
164100000000     C* CARMSG - CARICA MESSAGGIO NEL SUBFILE MESSAGGI E LO INVIA ALLA
164200000000     C*          CODA MESSAGGI DEL PGM
164300000000     C*------------------------------------------------------------------------*
164400000000     C     CARMSG        BEGSR
164500000000     C* SE QUELLO CHE STO PER CARICARE E' IL PRIMO MESSAGGIO
164600000000     C* PULISCO SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
164700000000     C     MSGULT        IFEQ      0
164800000000     C                   MOVEL     '1'           *IN40
164900110506     C                   WRITE     TLZ4DVK
165000000000     C                   MOVEL     '0'           *IN40
165100000000     C                   ENDIF
165200000000     C* INVIA MESSAGGIO ALLA CODA DEL PGM
165300000000     C                   MOVEL     $MSG          DSMSMS
165400000000     C                   CALL      'XMSG'        XMSGPA
165500000000     C     DSMSER        IFEQ      0
165600000000     C                   MOVEL     DSMSKE        VMCKEY                         *CHIAVE DEL MESSAGGI
165700000000     C                   ENDIF
165800000000     C* CARICA IL MESSAGGIO NEL SUBFILE MESSAGGI
165900000000     C                   ADD       1             MSGULT            4 0          *CHIAVE SFL MESS
166000110506     C                   WRITE     TLZ4DVM                                      *SFL MESS
166100000000     C* PULISCO CAMPO MESSAGGIO APPENA CARICATO
166200000000     C                   MOVEL     *BLANKS       $MSG
166300000000     C                   ENDSR
166400000000     C*------------------------------------------------------------------------*
166500000000     C* OUTMSG - EMETTE A VIDEO IL SUBFILE MESSAGGI
166600000000     C*------------------------------------------------------------------------*
166700000000     C     OUTMSG        BEGSR
166800000000     C* VISUALIZZAZIONE MESSAGGIO
166900000000     C                   MOVEL     '1'           *IN44                          *SFLEND (+)
167000110506     C                   WRITE     TLZ4DVK                                      *CONTR MESS
167100000000     C* AZZERO LA CHIAVE DEL SUBFILE MESSAGGI
167200000000     C                   Z-ADD     *ZEROS        MSGULT
167300000000     C                   ENDSR
167400000000     C*------------------------------------------------------------------------*
167500000000     C* CARFUN - CARICA LE SCHIERE PER LA GESTIONE DEI TASTI FUNZIONALI
167600000000     C*          INPUT:  - FUN    (SCHIERA TASTI ABILITATI PER IL VIDEO ATTUALE)
167700000000     C*          OUTPUT: - FFU    (SCHIERA FUNZIONI)
167800000000     C*                  - FDE    (SCHIERA DESCRIZIONI FUNZIONI)
167900000000     C*                  - FAT    (SCHIERA FLAG DI FUNZIONE ATTIVA S/N)
168000000000     C*------------------------------------------------------------------------*
168100000000     C     CARFUN        BEGSR
168200000000     C                   Z-ADD     *ZEROS        FUNULT            5 0          *ULTIMA FUNZ. VISUAL
168300000000     C     1             DO        31            I                 5 0          --- 1 -->
168400000000     C                   MOVEL     FUN(I)        FUNRIG                         *DS RIGA FUNZIONI
168500000000     C                   MOVEL     FUNFUN        FFU(I)                         *FUNZIONE
168600000000     C                   MOVEL     FUNDES        FDE(I)                         *DESCRIZIONE
168700020715     c                   If        DutLpo = 'S'
168800000000     C     $TGES         IFEQ      'G'                                          *GESTIONE
168900000000     C                   MOVEL     FUNASG        FAT(I)                         *FUNZIONE ATTIVA S/N
169000000000     C                   ELSE                                                   *INTERROG.
169100000000     C                   MOVEL     FUNASI        FAT(I)
169200000000     C                   ENDIF
169300000000     C                   ELSE                                                   <-- 2 --> *FILIALE
169400000000     C     $TGES         IFEQ      'G'                                          *GESTIONE
169500000000     C                   MOVEL     FUNAFG        FAT(I)
169600000000     C                   ELSE                                                   *INTERROG.
169700000000     C                   MOVEL     FUNAFI        FAT(I)
169800000000     C                   ENDIF
169900000000     C                   ENDIF                                                  <-- 2 ---
170000000000     C     FAT(I)        IFEQ      'E'                                          *FLAG 'E'->SOLO EDP
170100000000     C                   MOVEL     USER          WEDP              3            *PROFILO
170200000000     C     WEDP          IFEQ      'EDP'                                        *SE PROFILO EDPXX
170300000000     C                   MOVEL     'S'           FAT(I)                         *ABILITO
170400000000     C                   ELSE
170500000000     C                   MOVEL     'N'           FAT(I)                         *NON ABILITO
170600000000     C                   ENDIF
170700000000     C                   ENDIF
170800000000     C                   ENDDO                                                  <-- 1 ---
170900000000     C                   ENDSR
171000000000     C*------------------------------------------------------------------------*
171100000000     C* OUTFUN - EMETTE A VIDEO LE FUNZIONI ABILITATE
171200000000     C*          INPUT:  - FUNULT (ULTIMA FUNZIONE EMESSA NEL VIDEO PRECEDENTE)
171300000000     C*          OUTPUT: - VFD001-008 (CAMPI FUNZIONI A VIDEO)
171400000000     C*                  - FUNULT (ULTIMA FUNZIONE EMESSA A VIDEO)
171500000000     C*------------------------------------------------------------------------*
171600000000     C     OUTFUN        BEGSR
171700000000     C     1             DO        8             I                              *PULIZIA FUNZIONI A
171800000000     C                   MOVEL     *BLANKS       FUV(I)                         *VIDEO
171900000000     C                   ENDDO
172000000000     C* CONTROLLO QUANTE FUNZIONI ATTIVE DA EMETTERE A VIDEO CI SONO
172100000000     C                   Z-ADD     *ZEROS        FUNNAT            2 0          *N.FUNZIONI ATTIVE
172200000000     C     1             DO        31            I
172300000000     C     FAT(I)        IFEQ      'S'                                          *FUNZIONE ATTIVA
172400000000     C     FDE(I)        ANDNE     *BLANKS                                      *DESCRIZIONE FUNZION
172500000000     C                   ADD       1             FUNNAT
172600000000     C                   ENDIF
172700000000     C                   ENDDO
172800000000     C* SE VI SONO PIU' DI 8 FUNZIONI ATTIVO IL TASTO F24
172900000000     C     FUNNAT        IFGT      8
173000000000     C                   MOVEL     'S'           FAT(24)                        *FLAG ATTIVITA' FUNZ
173100000000     C                   ENDIF
173200000000     C* IMPOSTO IL N.DI FUNZIONI VISUALIZZABILI A 8, E SE SONO ATTIVE F23 E F24
173300000000     C* (SE ATTIVE DA VISUALIZZARE SEMPRE) DIVENTANO 6
173400000000     C                   Z-ADD     8             FUNVIS            5 0
173500000000     C     FAT(23)       IFEQ      'S'
173600000000     C                   SUB       1             FUNVIS
173700000000     C                   ENDIF
173800000000     C     FAT(24)       IFEQ      'S'
173900000000     C                   SUB       1             FUNVIS
174000000000     C                   ENDIF
174100000000     C* AGGIUNGO UNO ALL'ULTIMA FUNZIONE VISUALIZZATA PER AVERE LA 1A DA VISUALI
174200000000     C                   Z-ADD     *ZEROS        W                 5 0
174300000000     C                   ADD       1             FUNULT
174400000000     C     FUNULT        IFGT      31
174500000000     C                   Z-ADD     1             FUNULT
174600000000     C                   ENDIF
174700000000     C* RICERCO LE OPZIONI ATTIVE E LE EMETTO A VIDEO
174800000000     C     FUNULT        DO        31            I                              --- 1 -->
174900000000     C* SE HO GIA' RAGGIUNTO IL NUMERO DI FUNZIONI VISUALIZZABILI VADO A FINE
175000000000     C     W             IFLT      FUNVIS                                       --- 2 -->
175100000000     C* SE OPZIONE ATTIVA ED ESISTE LA DESCRIZIONE EMETTO A VIDEO
175200000000     C     FAT(I)        IFEQ      'S'                                          --- 3 -->
175300000000     C     FDE(I)        ANDNE     *BLANKS
175400000000     C     I             IFEQ      23                                           *TOLGO F23
175500000000     C     I             OREQ      24                                           *E F24
175600000000     C                   ELSE
175700000000     C                   ADD       1             W
175800000000     C                   MOVEL     FDE(I)        FUV(W)
175900000000     C                   ENDIF
176000000000     C                   ENDIF                                                  <-- 3 ---
176100000000     C* MEMORIZZO L'ULTIMO INDICE TRATTATO
176200000000     C                   Z-ADD     I             FUNULT
176300000000     C                   ENDIF                                                  <-- 2 ---
176400000000     C                   ENDDO                                                  <-- 1 ---
176500000000     C* TRASFERISCO LE OPZIONI DALLA SCHIERA AI CAMPI VIDEO
176600000000     C                   MOVEL     FUV(1)        VFD001
176700000000     C                   MOVEL     FUV(2)        VFD002
176800000000     C                   MOVEL     FUV(3)        VFD003
176900000000     C                   MOVEL     FUV(4)        VFD004
177000000000     C                   MOVEL     FUV(5)        VFD005
177100000000     C                   MOVEL     FUV(6)        VFD006
177200000000     C     FAT(23)       IFEQ      'N'                                          --- 1 -->
177300000000     C     FAT(24)       ANDEQ     'N'
177400000000     C                   MOVEL     FUV(7)        VFD007
177500000000     C                   MOVEL     FUV(8)        VFD008
177600000000     C                   ELSE                                                   <-- 1 -->
177700000000     C     FAT(23)       IFEQ      'S'                                          --- 2 -->
177800000000     C     FAT(24)       ANDEQ     'S'
177900000000     C                   MOVEL     FDE(23)       VFD007
178000000000     C                   MOVEL     FDE(24)       VFD008
178100000000     C                   ELSE                                                   <-- 2 -->
178200000000     C                   MOVEL     FUV(7)        VFD007
178300000000     C     FAT(23)       IFEQ      'S'
178400000000     C                   MOVEL     FDE(23)       VFD008
178500000000     C                   ELSE
178600000000     C                   MOVEL     FDE(24)       VFD008
178700000000     C                   ENDIF
178800000000     C                   ENDIF                                                  <-- 2 ---
178900000000     C                   ENDIF                                                  <-- 1 ---
179000000000     C                   MOVEL     '1'           $TESVD                         *RIEMETTE TESTATA VI
179100000000     C                   ENDSR
179200000000     C*------------------------------------------------------------------------*
179300000000     C*  CONFUN - CONTROLLO SE LA FUNZIONE SCELTA E' ATTIVA
179400000000     C*           INPUT:  - KEY    (FUNZIONE PREMUTA - DA INFDS)
179500000000     C*           OUTPUT: - FUNOK  (OPZIONE OK S/N)
179600000000     C*                   - ERRORE SE FUNZIONE NON ATTIVA
179700000000     C*------------------------------------------------------------------------*
179800000000     C     CONFUN        BEGSR
179900000000     C* RICERCA FUNZIONE SCELTA IN TABELLA FUNZIONI
180000000000     C     1             DO        31            I                              --- 1 -->
180100000000     C     KEY           IFEQ      KEYS(I)                                      --- 2 -->
180200000000     C* CONTROLLO SE IL TASTO E' ATTIVO
180300000000     C     FAT(I)        IFNE      'S'                                          --- 3 -->
180400000000     C                   MOVEL     MSG(1)        $MSG                           *FUNZ.NON ACCETTABIL
180500000000     C                   EXSR      CARMSG
180600000000     C                   MOVEL     '1'           $ERR
180700000000     C                   ENDIF                                                  <-- 3 ---
180800000000     C                   ENDIF                                                  <-- 2 ---
180900000000     C                   ENDDO                                                  <-- 1 ---
181000000000     C                   ENDSR
181100000000     C*------------------------------------------------------------------------*
181200000000** - MSG MESSAGGI                                                          .
181300110506TNTLZ4R- Tasto funzionale non valido.                                          1
181400110506TNTLZ4R- Tasto funzionale abilitato ma non gestito. Avvertire CED.             2
181500110506TNTLZ4R- Opzione non valida.                                                   3
181600000000Ritorno senza aggiornamento: premere nuovamente F12 per confermare l'uscita   70
181700110509TNTLZ4R- Il punto di scambio è stato registrato                               05
181800110509TNTLZ4R- Codice punti di scambio obbligatorio / errato                        06
181900110509TNTLZ4R- Numero traino di scambio obbligatorio / errato                       07
182000110509TNTLZ4R- Giorno di scambio errato                                             08
182100110509TNTLZ4R- Ora errata                                                           09
182200110509TNTLZ4R- Punti di scambio errato con scadenza minore di quella del traino     10
182300110510TNTLZ4R- Traino inesistente o scaduto                                         11
182400110510TNTLZ4R- Il punto di scambio è stato annullato                                12
182500110517Traino di scambio ANCORA privo del punto di scambio. Enter per continuare     13
182600110513Traino xxxxxxx  ha un traino di scambio differente, il xxxxxxx                14
182700110513Traino xxxxxxx prevede come traino di scambio il traino xxxxxxx in modifica   15
182800110608Giorno e ora scambio MINORI della prima fermata di carico. Enter per forzare  16
182900110608Giorno e ora scambio MAGGIORI dell'ult.fermata di scarico. enter per forzare  17
183000130320Punto di scambio(con filiale di ritorno) errato se previsto traino di scambio 18
183100130320Sono presenti degli errori.Cambiando gg verranno perse le modifiche           19
183200130320Ritorno senza aggiornamento: premere nuovamente F12 per confermare l'uscita   20
183300110509** - FUN  FUNZIONI        SEDE:GI FIL:GI   (G=GESTIONE,I=INTERROG.)
183400000000F1    -                   -    NN     NN
183500000000F2    -                   -    NN     NN
183600000000F3    -F3=Fine            -    NN     NN
183700000000F4    -                   -    NN     NN
183800000000F5    -                   -    NN     NN
183900000000F6    -F6=Conferma        -    SN     SN
184000110509F7    -                   -    NN     NN
184100000000F8    -                   -    NN     NN
184200000000F9    -F9=Cambia giorno   -    SS     SS
184300110506F10   -                   -    NN     NN
184400000000F11   -                   -    NN     NN
184500000000F12   -F12=Ritorno        -    SS     SS
184600000000F13   -                   -    NN     NN
184700000000F14   -                   -    NN     NN
184800000000F15   -                   -    NN     NN
184900110506F16   -F16=Annullamento   -    SN     SN
185000000000F17   -                   -    NN     NN
185100000000F18   -                   -    NN     NN
185200000000F19   -                   -    NN     NN
185300000000F20   -                   -    NN     NN
185400000000F21   -                   -    NN     NN
185500000000F22   -                   -    NN     NN
185600000000F23   -F23=Altre opzioni  -    NN     NN
185700000000F24   -F24=Altri tasti    -    NN     NN
185800000000ENTER -                   -    SS     SS
185900000000ROLLUP-                   -    SS     SS
186000000000ROLLDN-                   -    SS     SS
186100000000HELP  -                   -    NN     NN
186200000000PRINT -                   -    NN     NN
186300000000BSPACE-                   -    NN     NN
186400000000CLEAR -                   -    NN     NN
186500000000** -DGIO- DESCRIZIONE GIORNI DELLA SETTIMANA
186600000000LUN  1
186700000000MAR  2
186800000000MER  3
186900000000GIO  4
187000000000VEN  5
187100000000SAB  6
187200000000DOM  7
187300000000TUTTI8
187400000000** -DGES- DESCRIZIONE GIORNI DELLA SETTIMANA ESTESA
187500000000LUNEDI'         1
187600000000MARTEDI'        2
187700000000MERCOLEDI'      3
187800000000GIOVEDI'        4
187900000000VENERDI'        5
188000000000SABATO          6
188100000000DOMENICA        7
188200000000"TUTTI I GIORNI"8
188300000000** -DALF- ALFABETO
188400000000ABCDEFGHILMNOPQRSTUVZ
