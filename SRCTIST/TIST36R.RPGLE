000100041231     H DECEDIT('0,') DATEDIT(*DMY.)
000200980323     H*------------------------------------------------------------------------*
000300041231     H*  STATISTICA UTILIZZO SERVIZI VAS INTERNET                              *
000400980323     H*------------------------------------------------------------------------*
000500041231     FWFSUV02L  IF   E           K DISK
000600041231     FAZORG01L  IF   E           K DISK
000700041231     FTABEL00F  IF   E           K DISK
000800041231     FTIST36P   O    E             PRINTER OFLIND(*IN90)
000900050111     D*------------------------------------------------------------------------*
001000050111     D* SCHIERE
001100050111     D*------------------------------------------------------------------------*
001200050111     D*------------
001300050111     D* TOTOLI IN STAMPA
001400050111     D*------------
001500050111     D TIT             S             70    DIM(5) CTDATA PERRCD(1)
001600050111     D*------------
001700041231     D*-------------
001800041231     D* DS ESTERNE
001900041231     D*-------------
002000031105     D kpjba         e ds
002100041231     D*------------------
002200041231     D* SCHIERE ORGANIGRAMMA
002300041231     D*------------------
002400041231     D FIL             S              3  0 DIM(500)                             *FILIALI
002500041231     D DFIL            S             20    DIM(500)
002600041231     D ARE             S              3  0 DIM(500)                             *AREE
002700041231     D DARE            S             20    DIM(500)
002800041231     D DIV             S              1    DIM(500)                             *DIVISIONI
002900041231     D DDIV            S             20    DIM(500)
003000041231     D*-------------
003100041231     D* VARIABILI D WRK
003200041231     D*-------------
003300041231     D wDiv            S                   like(WFSDIV)
003400041231     D wAre            S                   like(WFSARE)
003500041231     D wFil            S                   like(WFSFIL)
003600041231     D jORG            S              3  0
003700041231     D wChkRec         S              1
003800041231     D*-------------
003900041231     D* PASSAGGIO PARAMETRI
004000041231     D*-------------
004100041231     D PARAM           DS
004200041231     D  PAREML                        1
004300041231     D  PARCAT                        1
004400041231     D  PARCATFLG                     1
004500041231     D  PARTIP                        2
004600041231     D  PARSPED                       6  0
004700041231
004800041231
004900031103
005000041231      *------------------------------------------------------------------------*
005100041231      * \MAIN
005200041231      *------------------------------------------------------------------------*
005300041231     C*
005400041231     C* CARICA I DATI TABELLATI
005500041231     C                   EXSR      CARTAB
005600041231     C*
005700041231     C* IMPOSTA INTESTAZIONE E PARAMETRI D LANCIO IN TESTATA
005800041231     C                   EXSR      IMPTESTATA
005900041231     C*
006000041231     C* ESEGUE ELABORAZIONE => STAMPA
006100041231     C                   EXSR      EXESTAMPA
006200041231     C*
006300041231     C                   SETON                                        LR
006400041231      *------------------------------------------------------------------------*
006500041231      * EXESTAMPA - ROUTINE STAMPA TABUALTI
006600041231      *------------------------------------------------------------------------*
006700041231     C     EXESTAMPA     BEGSR
006800041231     C*
006900050107     C* INIZIALIZZO INDICATORI E VARIABILI D WRK
007000041231     C                   clear                   wDiv
007100041231     C                   clear                   wAre
007200041231     C                   clear                   wFil
007300050107     C                   setoff                                       80
007400041231     C*
007500041231     C* LEGGO TUTTO IL FILE
007600041231     C     *loval        setll     WFSUV02L
007700041231     C                   read      WFSUV02L
007800050107     C*
007900050107     C* VALORIZZO SUBITO IL PERIODO D RIFERIMENTO SPEDIZIONI/FATTURATO
008000050107     C                   movel(p)  WFSDAI        wPARDAIalfa       6
008100050107     C                   movel(p)  WFSDAF        wPARDAFalfa       6
008200050107     C                   eval      wPARDAIalfa = %subst(wPARDAIalfa:5:2)+
008300050107     C                                           %subst(wPARDAIalfa:1:4)
008400050107     C                   eval      wPARDAFalfa = %subst(wPARDAFalfa:5:2)+
008500050107     C                                           %subst(wPARDAFalfa:1:4)
008600050107     C                   move(p)   wPARDAIalfa   PARDAI
008700050107     C                   move(p)   wPARDAFalfa   PARDAF
008800050107     C*
008900050107     C* PROSEGUO CON LA LETTURA DEL WRK-FILE
009000041231     C                   dow       not %eof(WFSUV02L)
009100050107     C*
009200050107     C* INIZIALIZZO I CAMPI D DETTAGLIO SOGGETTI A VALORIZZAZIONE CONDIZIONATA
009300050107     C                   clear                   STPFATT
009400041231     C*
009500041231     C* VERIFICO SE RECORD CORRENTE CORRISPONDE AI PARAMETRI D LANCIO
009600041231     C                   exsr      CHKREC
009700041231     C*
009800041231     C                   if        wChkRec = 'S'
009900041231     C*
010000041231     C* VERIFICO ROTTURA D CODICE X DIV/ARE/FIL
010100041231     C                   if        wDiv <> WFSDIV OR
010200041231     C                             wAre <> WFSARE OR
010300041231     C                             wFil <> WFSFIL
010400041231     C*
010500041231     C* SE ROTTURA DIV => REPERISCO NUOVA DESCRIZIONE
010600041231     C                   if        wDiv <> WFSDIV
010700041231     C                   exsr      DECDIV
010800041231     C                   eval      wDiv = WFSDIV
010900041231     C                   endif
011000041231     C*
011100041231     C* SE ROTTURA ARE => REPERISCO NUOVA DESCRIZIONE
011200041231     C                   if        wAre <> WFSARE
011300041231     C                   exsr      DECARE
011400041231     C                   eval      wAre = WFSARE
011500041231     C                   endif
011600041231     C*
011700041231     C* SE ROTTURA FIL => REPERISCO NUOVA DESCRIZIONE
011800041231     C                   if        wFil <> WFSFIL
011900041231     C                   exsr      DECFIL
012000041231     C                   eval      wFil = WFSFIL
012100041231     C                   endif
012200041231     C*
012300041231     C* STAMPO TESTATA
012400050107     C   80              write     ST36FIN
012500041231     C                   write     ST36TES
012600050107     C                   seton                                        80
012700041231     C                   setoff                                       90
012800041231     C                   endif
012900050103     C*
013000050103     C* EFFETTUO CONSIDERAZIONI SU DATI D DETTAGLIO
013100050107     C* ...FATTURATO SOLO SE SPEDIZIONI > 0
013200050107     C                   if        WFSSPED > *zeros
013300050107     C                   eval      STPFATT = WFSFATT
013400050107     C                   endif
013500050107     C* ...TIPOLOGIA CLIENTE
013600050107     C                   if        PARTIP = *blanks
013700050103     C                   if        WFSTIP = 'AU'
013800050107     C                   eval      STPNOTECLI = 'Il cliente utilizza i ' +
013900050103     C                             'servizi VAS internet'
014000050103     C                   endif
014100050103     C                   if        WFSTIP = 'AN'
014200050107     C                   eval      STPNOTECLI = 'Il cliente è abilitato ' +
014300050103     C                             'ai servizi VAS internet ma NON li utilizza'
014400050103     C                   endif
014500050103     C                   if        WFSTIP = 'NA'
014600050107     C                   eval      STPNOTECLI = 'Il cliente NON è abilitato ' +
014700050103     C                             'ai servizi VAS internet'
014800050103     C                   endif
014900050107     C                   endif
015000041231     C*
015100041231     C* STAMPO DETTAGLIO (VERIFICANDO OVERFLOW)
015200041231     C                   if        *in90
015300050107     C                   write     ST36FIN
015400041231     C                   write     ST36TES
015500041231     C                   eval      *in90=*off
015600041231     C                   endif
015700041231     C                   write     ST36DET
015800041231     C*
015900041231     C                   endif
016000041231     C*
016100041231     C* PROSEGUO NELLA LETTURA DEL FILE
016200041231     C                   read      WFSUV02L
016300041231     C                   enddo
016400041231     C*
016500050107     C* CHIUDO IL PROSPETTO
016600041231     C                   write     ST36FIN
016700041231     C                   write     ST36END
016800041231     C*
016900041231     C                   ENDSR
017000041231      *------------------------------------------------------------------------*
017100041231      * IMPTESTATA - ROUTINE D IMPOSTAZIONE TESTATA
017200041231      *------------------------------------------------------------------------*
017300041231     C     IMPTESTATA    BEGSR
017400041231     C*
017500041231     C                   if        PAREML = 'E'
017600041231     C                   eval      STPPARAM = %trim(STPPARAM) + '  ' +
017700041231     C                             '*Solo clienti con e-mail'
017800041231     C                   endif
017900041231     C                   if        PAREML = 'N'
018000041231     C                   eval      STPPARAM = %trim(STPPARAM) + '  ' +
018100041231     C                             '*Solo clienti senza e-mail'
018200041231     C                   endif
018300041231     C                   if        PARCAT <> *blanks
018400041231     C                   if        PARCATFLG = 'I'
018500041231     C                   eval      STPPARAM = %trim(STPPARAM) + '  ' +
018600041231     C                             '*Solo clienti categoria: '+ PARCAT
018700041231     C                   endif
018800041231     C                   if        PARCATFLG = 'E'
018900041231     C                   eval      STPPARAM = %trim(STPPARAM) + '  ' +
019000041231     C                             '*No clienti categoria: '+ PARCAT
019100041231     C                   endif
019200041231     C                   endif
019300050111     C                   if        PARTIP = *blanks
019400050111     C                   eval      STPTIT = TIT(4)
019500050111     C                   else
019600050111     C                   if        PARTIP =  'AN'
019700050111     C                   eval      STPTIT = TIT(1)
019800041231     C                   endif
019900050111     C                   if        PARTIP =  'AU'
020000050111     C                   eval      STPTIT = TIT(2)
020100050111     C                   endif
020200050111     C                   if        PARTIP =  'NA'
020300050111     C                   eval      STPTIT = TIT(3)
020400050111     C                   endif
020500041231     C                   endif
020600050107     C                   if        PARSPED <> *zeros
020700050107     C                   eval      STPPARAM = %trim(STPPARAM) + '  ' +
020800050107     C                             '*Solo clienti con spedizioni maggiori '+
020900050107     C                             'di: ' + %trim(%editc(PARSPED:'2'))
021000050107     C                   endif
021100041231     C*
021200041231     C                   ENDSR
021300041231      *------------------------------------------------------------------------*
021400041231      * CHKREC - ROUTINE VERIFICA RECORD
021500041231      *------------------------------------------------------------------------*
021600041231     C     CHKREC        BEGSR
021700041231     C*
021800041231     C* INIZIALIZZO IL FLAG D WRK
021900041231     C                   movel     'S'           wChkRec
022000050107     C*
022100050107     C* SOLO SE DISTRETTO <> *BLANKS
022200050107     C                   if        wChkRec =  'S'
022300050107     C                   if        WFSDIV  <  '1'
022400050107     C                   movel     'N'           wChkRec
022500050107     C                   endif
022600050107     C                   endif
022700041231     C*
022800041231     C* VERIFICA E-MAIL
022900041231     C                   if        wChkRec =  'S'
023000041231     C                   if        PAREML  <> *blanks
023100041231     C                   if        PAREML  =  'E' and WFSEMAIL = *blanks
023200041231     C                   movel     'N'           wChkRec
023300041231     C                   endif
023400041231     C                   if        PAREML  =  'N' and WFSEMAIL <> *blanks
023500041231     C                   movel     'N'           wChkRec
023600041231     C                   endif
023700041231     C                   endif
023800041231     C                   endif
023900041231     C*
024000041231     C* VERIFICA CATEGORIA CLIENTE
024100041231     C                   if        wChkRec =  'S'
024200041231     C                   if        PARCAT  <> *blanks
024300041231     C                   if        PARCATFLG = 'I'     AND
024400041231     C                             PARCAT    <> WFSCAT
024500041231     C                   movel     'N'           wChkRec
024600041231     C                   endif
024700041231     C                   if        PARCATFLG = 'E'     AND
024800041231     C                             PARCAT    = WFSCAT
024900041231     C                   movel     'N'           wChkRec
025000041231     C                   endif
025100041231     C                   endif
025200041231     C                   endif
025300041231     C*
025400041231     C* VERIFICA TIPOLOGIA CLIENTE
025500041231     C                   if        wChkRec =  'S'
025600041231     C                   if        PARTIP  <> *blanks
025700041231     C                   if        PARTIP  <> WFSTIP
025800041231     C                   movel     'N'           wChkRec
025900041231     C                   endif
026000041231     C                   endif
026100041231     C                   endif
026200041231     C*
026300041231     C* VERIFICA SBARRAMENTO SPEDIZIONI
026400041231     C                   if        wChkRec =  'S'
026500041231     C                   if        PARSPED <> *zeros
026600041231     C                   if        WFSSPED <  PARSPED
026700041231     C                   movel     'N'           wChkRec
026800041231     C                   endif
026900041231     C                   endif
027000041231     C                   endif
027100041231     C*
027200041231     C                   ENDSR
027300041231      *------------------------------------------------------------------------*
027400041231      *?DECDIV - DECODIFICA CODICE DIVISIONE
027500041231     C*------------------------------------------------------------------------*
027600041231     C     DECDIV        BEGSR
027700041231     C*
027800041231     C                   Z-ADD     1             jORG
027900041231     C     WFSDIV        LOOKUP    DIV(jORG)                              99
028000041231     C     *IN99         IFEQ      *OFF
028100050103     C                   MOVEL(P)  *ALL'*'       STPDIVD
028200041231     C                   ELSE
028300050103     C                   MOVEL(P)  DDIV(jORG)    STPDIVD
028400041231     C                   ENDIF
028500041231     C*
028600041231     C                   ENDSR
028700041231      *------------------------------------------------------------------------*
028800041231      *? DECARE - DECODIFICA CODICE AREA
028900041231     C*------------------------------------------------------------------------*
029000041231     C     DECARE        BEGSR
029100041231     C*
029200041231     C                   Z-ADD     1             jORG
029300041231     C     WFSARE        LOOKUP    ARE(jORG)                              99
029400041231     C     *IN99         IFEQ      *OFF
029500050103     C                   MOVEL(P)  *ALL'*'       STPARED
029600041231     C                   ELSE
029700050103     C                   MOVEL(P)  DARE(jORG)    STPARED
029800041231     C                   ENDIF
029900041231     C*
030000041231     C                   ENDSR
030100041231      *------------------------------------------------------------------------*
030200041231      *? DECFIL - DECODIFICA CODICE FILIALE
030300041231      *------------------------------------------------------------------------*
030400041231     C     DECFIL        BEGSR
030500041231     C*
030600041231     C                   Z-ADD     1             jORG
030700041231     C     WFSFIL        LOOKUP    FIL(jORG)                              99
030800041231     C     *IN99         IFEQ      *OFF
030900050103     C                   MOVEL(P)  *ALL'*'       STPFILD
031000041231     C                   ELSE
031100050103     C                   MOVEL(P)  DFIL(jORG)    STPFILD
031200041231     C                   ENDIF
031300041231     C*
031400041231     C                   ENDSR
031500041231      *------------------------------------------------------------------------*
031600041231      *?CARTAB - CARICA I DATI IN TABELLE
031700041231      *------------------------------------------------------------------------*
031800041231     C     CARTAB        BEGSR
031900041231     C*--------------------
032000041231     C* FILIALI/AREE/DIVISIONI
032100041231     C*--------------------
032200041231     C*
032300041231     C* POSIZIONAMENTO SUL FILE E PRIMA LETTURA
032400041231     C     *LOVAL        SETLL     AZORG01L
032500041231     C                   READ      AZORG01L                               99
032600041231     C*
032700041231     C* CICLO FINO A FINE FILE
032800041231     C                   Z-ADD     *ZEROS        jORG
032900041231     C     *IN99         DOWEQ     *OFF
033000041231     C*
033100041231     C* NN CONSIEDERA RECORD ANNULLATI
033200041231     C     ORGFVA        IFEQ      *BLANKS
033300041231     C                   ADD       1             jORG
033400041231     C* F I L I A L I
033500041231     C                   Z-ADD     ORGFIL        FIL(jORG)
033600041231     C                   MOVEL     ORGDES        DFIL(jORG)
033700041231     C* A R E E
033800041231     C                   Z-ADD     1             TBLKUT
033900041231     C                   MOVEL     '05'          TBLCOD
034000041231     C                   MOVEL(P)  ORGCAR        TBLKEY
034100041231     C     KEYTAB        CHAIN     TABEL00F                           99
034200041231     C     *IN99         IFEQ      *OFF
034300041231     C                   Z-ADD     ORGCAR        ARE(jORG)
034400041231     C                   MOVEL     TBLUNI        DARE(jORG)
034500041231     C                   ENDIF
034600041231     C* D I V I S I O N I
034700041231     C                   Z-ADD     1             TBLKUT
034800041231     C                   MOVEL     '17'          TBLCOD
034900041231     C                   MOVEL(P)  ORGFL3        TBLKEY
035000041231     C     KEYTAB        CHAIN     TABEL00F                           99
035100041231     C     *IN99         IFEQ      *OFF
035200041231     C                   MOVEL     ORGFL3        DIV(jORG)
035300041231     C                   MOVEL     TBLUNI        DDIV(jORG)
035400041231     C                   ENDIF
035500041231     C                   ENDIF
035600041231     C*
035700041231     C* LEGGE IL RECORD SUCCESSIVO
035800041231     C                   READ      AZORG01L                               99
035900041231     C                   ENDDO
036000041231     C*
036100041231     C                   ENDSR
036200041231      *------------------------------------------------------------------------*
036300041231      * *INZSR - ROUTINE INIZIALE
036400041231      *------------------------------------------------------------------------*
036500031103     C     *INZSR        BEGSR
036600041231     C*
036700031105     C     *ENTRY        PLIST
036800031105     C                   PARM                    KPJBA
036900041231     C                   MOVEL(P)  KPJBU         PARAM
037000041231     C*--------------------
037100041231     C* CHAIVI DI LETTURA
037200041231     C*--------------------
037300041231     C* LETTURA TABEL00F - COMPLETA
037400041231     C     KEYTAB        KLIST
037500041231     C                   KFLD                    TBLKUT
037600041231     C                   KFLD                    TBLCOD
037700041231     C                   KFLD                    TBLKEY
037800041231     C*
037900041231     C* REPERIMENTO DATA E ORA CORRENTE
038000031110     C                   TIME                    WTIME            14 0
038100031110     C                   MOVE      WTIME         WDATE             8 0
038200031110     C                   MOVEL     WTIME         UTIME             6 0
038300041231     C*
038400041231     C                   ENDSR
038500050111     O*------------------------------------------------------------------------*
038600050111** TIT - DECODIFICHE
038700050111     ELENCO CLIENTI ABILITATI AI SERVIZI ON LINE (NON UTILIZZANO)     1
038800050111      ELENCO CLIENTI ABILITATI AI SERVIZI ON LINE (UTILIZZANO)        2
038900050111          ELENCO CLIENTI NON ABILITATI AI SERVIZI ON LINE             3
039000050111                   ELENCO CLIENTI SERVIZI ON LINE                     4
039100050111                                                                      5
