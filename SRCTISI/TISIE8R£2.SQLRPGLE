000100970526     H*------------------------------------------------------------------------*
000200061002     H* Interrogazione cappario DPd Geo Post
000300970526     H*------------------------------------------------------------------------*
000400970526     H DECEDIT('0,') DATEDIT(*DMY.)
000500970526     F*------------------------------------------------------------------------*
000600970526     F* DATA BASE
000700970526     F*------------------------------------------------------------------------*
000800061002     FTISIE8D   CF   E             WORKSTN INFDS(DEVDS1)
000900061002     F                                     SFILE(SIE8D05:SFLULT)
001000061002     F                                     SFILE(SIE8DMS:MSGULT)
001100060922     Ftabel00f  IF   E           K DISK
001200061009     Fdpcve01l  IF   E           K DISK
001300970526     D*------------------------------------------------------------------------*
001400970526     D* SCHIERE
001500970526     D*------------------------------------------------------------------------*
001600061010     D*------------
001700970526     D* DECODIFICHE
001800970526     D*------------
001900970526     D DEC             S             66    DIM(5) CTDATA PERRCD(1)
002000970526     D*------------
002100970526     D* MESSAGGI
002200970526     D*------------
002300970526     D MSG             S             78    DIM(20) CTDATA PERRCD(1)
002400970526     D*------------
002500970526     D* DESCRIZIONI OPZIONI
002600970526     D*------------
002700970526     D OPD             S             80    DIM(24) CTDATA PERRCD(1)
002800970526     D*------------
002900970526     D* DESCRIZIONI TASTI FUNZIONALI
003000970526     D*------------
003100970526     D FUD             S             80    DIM(24) CTDATA PERRCD(1)
003200970526     D*------------
003300970526     D* OPZIONI DA CARICARE A VIDEO
003400970526     D*------------
003500970526     D OPV             S             19    DIM(24)
003600970526     D*------------
003700970526     D* FUNZIONI DA CARICARE A VIDEO
003800970526     D*------------
003900970527     D FUV             S             19    DIM(24)
004000061010     D*------------
004100061010     d* Schiere uso programma
004200061010     D*------------
004300061010     D CCP             S              1    DIM(9)                               CARATT.CONTROLLO
004400061010     D CAP             S              1    DIM(9)                               CAP DA CONTROLLAR
004500061010     D CAF             S              1    DIM(9)                               CAP FITTIZIO
004600061010     d
004700970526     D*------------------------------------------------------------------------*
004800970526     D* INPUT
004900970526     D*------------------------------------------------------------------------*
005000970526     D* RIDEFINIZIONE OPZIONI VIDEO
005100970526     D OPDDS           DS
005200970526     D  OPDOPZ                 1      2
005300970526     D  OPDDE1                 3     19
005400970526     D  OPDOP1                21     22
005500970526     D  OPDFI0                23     34
005600970526     D  OPDATT                35     35
005700970526     D  OPDFI1                36     80
005800970526     D  OPDDES                 1     19
005900970526     D  OPDRIG                 1     80
006000970526     D*------------
006100970526     D* RIDEFINIZIONE FUNZIONI VIDEO
006200970526     D FUDDS           DS
006300970526     D  FUDDE1                 1      1
006400970526     D  FUDFUN                 2      3
006500970526     D  FUDDE2                 4     19
006600970526     D  FUDFI0                20     34
006700970526     D  FUDATT                35     35
006800970526     D  FUDFI1                36     80
006900970526     D  FUDDES                 1     19
007000970526     D  FUDRIG                 1     80
007100970526     D*------------
007200970526     D* DS GESTIONE TASTI FUNZIONALI
007300970526     D DSKEY         E DS
007400970526     D DEVDS1          DS
007500970526     D  KEY                  369    369
007600970526     D*------------
007700970526     D* DS "XMSG" - GESTIONE MESSAGGI
007800970526     D*------------
007900970526     D DSMSG         E DS
008000970526     D*------------
008100020919     D TIBS34DS      E DS                                                       *Profili utente
008200020919     D DDATIUTE      E DS                                                       *Dati utente
008300020919     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
008400970527     D*------------------
008500970527     D WLBDA8          DS                  INZ
008600970527     D  G08DAT                 1      8  0
008700970527     D  G08INV                 9     16  0
008800970527     D  G08ERR                17     17
008900970527     D  G08TGI                18     22  0
009000970526     D*------------
009100970526     D* DS DI PROCEDURA
009200970526     D*------------
009300061002     D tiSIE8ds      E DS
009400061002     D tisiE2ds      E DS
009500061011     D tisi95ds      E DS
009600970526     D*------------
009700061009     D DS15          E DS
009800061009     D KPJBA         E DS
009900061009     D DPCRODS       E DS                  extname(dpcro00f)
010000060313     d
010100060313     d Dataiso         s               d   datfmt(*iso)
010200060313     d Dataeur         s               d   datfmt(*eur)
010300060329     d Datadmy         s               d   datfmt(*dmy)
010400060404     d opzione         s              2
010500061011     d posin           s              2  0
010600061011     d lung            s              2  0
010700061009     d wrkgetlista     s           4096    varying
010800061009     d kcod            s                   like(tblcod)
010900061009     d kkey            s                   like(tblkey)
011000970526     C*------------------------------------------------------------------------*
011100970526     C* MAIN LINES
011200970526     C*------------------------------------------------------------------------*
011300970526     C*
011400970526     C* SE NON CI SONO ERRORI, PROSEGUE
011500970526IF  1C     ERRL00        IFNE      '1'
011600970526     C*
011700970526     C* CARICA IL SFILE
011800970526     C                   EXSR      POSSFL
011900970526     C*
012000970526     C* CICLO FINO A CHE NON VIENE PREMUTO F03 O F12
012100970526DO  2C     F03L00        DOUEQ     '1'                                          *F03 PREMUTO
012200970526     C     F12L00        OREQ      '1'                                          *F12 PREMUTO
012300970526     C     FINL00        OREQ      '1'                                          *FINE PGM
012400970526     C*
012500970526     C* REIMPOSTA ERRORE DI PROCERURA
012600970526     C                   MOVEL     '0'           ERRL00
012700970526     C*
012800970526     C* EMETTE IL SFILE
012900970526     C                   EXSR      OUTSFL
013000970526     C*
013100970526     C* GESTIONE TASTI FUNZIONALI
013200970526     C                   EXSR      FUNCON
013300970526     C*
013400970526E   2C                   ENDDO
013500970527     C*
013600970527     C* CI SONO ERRORI IN ENTRATA
013700970527X   1C                   ELSE
013800061009     C                   MOVEL     '1'           osiE8ERR
013900061009     C                   MOVEL     DSMSMS        osiE8MSG
014000970526E   1C                   ENDIF
014100970526     C* FINE PGM
014200061009     c                   if        apricursor='S'
014300061009     c                   exsr      CHIUSQL
014400061009     c                   clear                   apricursor        1
014500061009     c                   endif
014600061009     c
014700970526     C                   SETON                                        LR
014800970526     C*------------------------------------------------------------------------*
014900970526     C* F03GES - GESTIONE F03 -> FINE PROGRAMMA
015000970526     C*------------------------------------------------------------------------*
015100970526     C     F03GES        BEGSR
015200970526     C*
015300970526     C* IMPOSTA "PREMUTO F03"
015400970526     C                   MOVEL     '1'           F03L00                         *A PROGRAMMA
015500061009     C                   MOVEL     '1'           osiE8F03                       *DI PROCEDURA
015600970526     C*
015700970526     C                   ENDSR
015800970526     C*------------------------------------------------------------------------*
015900970526     C* F05GES - GESTIONE F05 -> RIVISUALIZZA
016000970526     C*------------------------------------------------------------------------*
016100970526     C     F05GES        BEGSR
016200970526     C*
016300970526     C                   EXSR      POSSFL                                       *RICARICA SFILE
016400970526     C*
016500970526     C                   ENDSR
016600970526     C*------------------------------------------------------------------------*
016700970526     C* F06GES - GESTIONE F06 -> INSERIMENTO
016800970526     C*------------------------------------------------------------------------*
016900970526     C     F06GES        BEGSR
017000970526     C*
017100970526     C                   ENDSR
017200060926     C*------------------------------------------------------------------------*
017300060926     C* F07GES - GESTIONE F07 -> int cappario
017400060926     C*------------------------------------------------------------------------*
017500060926     C     F07GES        BEGSR
017600060926     C                   ENDSR
017700970526     C*------------------------------------------------------------------------*
017800970526     C* F12GES - GESTIONE F12 -> RITORNO
017900970526     C*------------------------------------------------------------------------*
018000970526     C     F12GES        BEGSR
018100970526     C*
018200970526     C* IMPOSTA "PREMUTO F12"
018300970526     C                   MOVEL     '1'           F12L00                         *A PROGRAMMA
018400061009     C                   MOVEL     '1'           osie8f12                       *DI PROCEDURA
018500970526     C*
018600970526     C                   ENDSR
018700970529     C*------------------------------------------------------------------------*
018800060922     C* F13GES - GESTIONE F13 ->
018900970529     C*------------------------------------------------------------------------*
019000970529     C     F13GES        BEGSR
019100970529     C*
019200970529     C                   ENDSR
019300970526     C*------------------------------------------------------------------------*
019400970526     C* F23GES - GESTIONE F23 -> ALTRE OPZIONI
019500970526     C*------------------------------------------------------------------------*
019600970526     C     F23GES        BEGSR
019700970526     C*
019800970526     C                   EXSR      OPZGES                                       *GESTIONE OPZIONI
019900970526     C*
020000970526     C                   ENDSR
020100970526     C*------------------------------------------------------------------------*
020200970526     C* F24GES - GESTIONE F24 -> ALTRE FUNZIONI
020300970526     C*------------------------------------------------------------------------*
020400970526     C     F24GES        BEGSR
020500970526     C*
020600970526     C                   EXSR      FUNGES                                       *GESTIONE FUNZIONI
020700970526     C*
020800970526     C                   ENDSR
020900970526     C*------------------------------------------------------------------------*
021000970526     C* RUPGES - GESTIONE RU -> PAGINA SUCCESSIVA
021100970526     C*------------------------------------------------------------------------*
021200970526     C     RUPGES        BEGSR
021300970526     C*
021400970526     C                   EXSR      CARSFL                                       *LEGGE DATABASE
021500970526     C*
021600970526     C                   ENDSR
021700970526     C*------------------------------------------------------------------------*
021800970526     C* ENTGES - GESTIONE ENTER -> INVIO
021900970526     C*------------------------------------------------------------------------*
022000970526     C     ENTGES        BEGSR
022100970526     C*
022200970526     C* CONTROLLA SE RICHIESTO NUOVO POSIZIONAMENTO
022300970526     C                   EXSR      NEWPOS
022400970526     C*
022500970526     C* SWPOSI-> 0=NO NUOVO OSIZIONAMENTO, 1=NUOVO POSIZIONAMENTO
022600970526IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
022700970526IF  2C     SWPOSI        IFNE      0
022800970526     C                   EXSR      POSSFL                                       *RICARICA SFILE
022900970526X   2C                   ELSE
023000970526IF  3C     SFLULT        IFNE      0
023100970526     C                   EXSR      ENTATT                                       *GESTIONE ENTER
023200970526     C*
023300970526     C* SWMODI-> 0=NO RICARICA SFILE, 1=RICARICA SFILE
023400970526IF  4C     SWMODI        IFNE      0
023500970526     C                   EXSR      POSSFL
023600970526E   4C                   ENDIF
023700970526E   3C                   ENDIF
023800970526E   2C                   ENDIF
023900970526E   1C                   ENDIF
024000970526     C*
024100970526     C                   ENDSR
024200970526     C*------------------------------------------------------------------------*
024300970526     C* O01GES - GESTIONE OPZIONE 1=SCELTA
024400970526     C*------------------------------------------------------------------------*
024500970526     C     O01GES        BEGSR
024600970526     C*
024700970526     C                   MOVEL     '1'           FINL00                         *FINE PGM
024800061011     C*
024900061010     c                   movel     v5ciso        osie8iso2
025000061010     c                   movel     v5cver        osie8ver
025100061010     c                   movel     v5cptcb       osie8ptcb
025200061010     c                   movel     v5cptce       osie8ptce
025300061010     c                   movel     v5cddep       osie8ddep
025400061010     c
025500061010     c* CAP "normalizzato" bartolini
025600061011     c* Se c'e' uguale potengo così
025700061011     C                   MOVEl     v5cptcb       WCAPFI            9            *MEM CAP FITTIZIO
025800061011     c                   exsr      LETCAP
025900061011     c
026000061011     c                   if        o95err=*blanks
026100061011     c                   movel     v5cptcb       osie8capb
026200061011     c                   else
026300061010     c* Recupero cap fittizio
026400061010     C                   MOVEA     v5cptcb       CAP
026500061011     C                   MOVEA     v5cptcb       CCP
026600061010     C*
026700061010     C* RECUPERO CAP FITTIZIO PER EVENTUALE TENTATIVO SUCCESSIVO
026800061010     C                   Z-ADD     §15PCF        P                 2 0
026900061010     C                   CLEAR                   CAF
027000061010     C                   MOVEA     CAP(P)        CAF(P)
027100061010     C                   ADD       §15ECF        P                              *CARATT.DA TENERE
027200061010     c
027300061010     c* Addiziono 1 al cal fittizio perchè nel loop sotto
027400061010     c*  di controllo parte sottraendo 1 carattere al cap fittizio
027500061010     c                   add       1             p
027600061010     c
027700061010     C     P             IFLT      10
027800061010     C                   MOVEA     *BLANKS       CAF(P)
027900061010     C                   ENDIF
028000061010     c
028100061010     c                   eval      o95err='E'
028200061010     c
028300061010     c* Cerco se esiste su cappario
028400061011     c                   dow       o95err<>*blanks and p>§15pcf
028500061010     c* tolto 1 carattere per il fittizio
028600061010     c                   sub       1             p
028700061010     c                   movel     *blanks       caf(P)
028800061010     C                   MOVEA     CAF           WCAPFI            9            *MEM CAP FITTIZIO
028900061010     c
029000061010     c                   exsr      LETCAP
029100061010     c*
029200061011     c                   enddo
029300061010     c
029400061010     c*Errore : non trovato nostro cap fittizio
029500061011     c                   if        p=§15pcf and o95err<>' '
029600061011     C                   MOVEL     MSG(7)        osie8msg
029700061011     C                   MOVEL     '1'           osie8err
029800061010     c                   else
029900061011     c* Scrivo il cap modo bartolini
030000061011     c* 1° PARTE DEL CAP SE FITTIZIO NON INIZIa DALLA POSIZIONE 1
030100061011     C                   IF        §15PCF>1
030200061011     c                   movea     *blanks       ccp(§15pcf)
030300061011     c                   movea     ccp           osie8capb
030400061011     c                   else
030500061011     c                   clear                   osie8capb
030600061011     C                   ENDIF
030700061011     c*
030800061011     c* 2° PARTE --> cap fittizio
030900061011     c                   eval      %subst(osie8capb:§15pcf:§15ecf)=
031000061011     c                             %subst(wcapfi:§15pcf:§15ecf)
031100061011     c* 3° PARTE --> parte finale del cap
031200061011     c                   eval      lung= (9-§15pcf-§15ecf+1)
031300061011     c                   eval      posin=(§15pcf+§15ecf)
031400061011     c*
031500061011     c                   clear                   finecap
031600061011     c                   movea     ccp(P)        finecap           9
031700061011     c                   eval      %subst(osie8capb:posin:lung)=finecap
031800061010     c                   endif
031900061011     c                   endif
032000970526     C*
032100970526     C                   ENDSR
032200061010     c*--------------------------------------------------------------------------------------------*
032300061010     C* Lettura con cap per verificare se esiste il fittizio
032400061010     c*--------------------------------------------------------------------------------------------*
032500061010     c     LETCAP        BEGSR
032600061010     c* richiamo il tisi95 pasato il cap
032700061010     c                   clear                   tisi95ds
032800061011     c                   movel     '3'           i95tcn
032900061010     C                   MOVEL     wcapfi        I95CAP
033000061010     C                   MOVEL     vidpna        I95NAR
033100061010     C                   MOVEL     amgpde        I95DAT
033200061010     c                   call      'TISI95R'
033300061010     C                   parm                    tisi95ds
033400061010c
033500061010     c                   ENDSR
033600970526     C*------------------------------------------------------------------------*
033700970526     C* O02GES - GESTIONE OPZIONE 2=MODIFICA
033800970526     C*------------------------------------------------------------------------*
033900970526     C     O02GES        BEGSR
034000970526     C*
034100970526     C                   ENDSR
034200970611     C*------------------------------------------------------------------------*
034300970611     C* O03GES - GESTIONE OPZIONE 3=DUPLICA
034400970611     C*------------------------------------------------------------------------*
034500970611     C     O03GES        BEGSR
034600970611     C                   ENDSR
034700970526     C*------------------------------------------------------------------------*
034800970526     C* O04GES - GESTIONE OPZIONE 4=ANNULLAMENTO
034900970526     C*------------------------------------------------------------------------*
035000970526     C     O04GES        BEGSR
035100970526     C                   ENDSR
035200970526     C*------------------------------------------------------------------------*
035300970526     C* O05GES - GESTIONE OPZIONE 5=VISUALIZZAZIONE
035400970526     C*------------------------------------------------------------------------*
035500970526     C     O05GES        BEGSR
035600970529     C                   ENDSR
035700970526     C*------------------------------------------------------------------------*
035800970526     C* NEWPOS - CONTROLLA RICHIESTA NUOVO POSIZIONAMENTO
035900970526     C*------------------------------------------------------------------------*
036000970526     C     NEWPOS        BEGSR
036100970526     C*
036200970526     C* SWPOSI: 0= NESSUNA RICHIESTA DI POSIZIONAMENTO
036300970526     C*         1= RICHIESTO POSIZIONAMENTO
036400970526     C                   Z-ADD     0             SWPOSI            7 0
036500970526     C*
036600970526     C* SWMODI: 0= NESSUN AGGIORNAMENTO AFFETTUATO SUL FILE DALL'UTENTE
036700970526     C*            CON L'OPZIONE 2, NON OCCORRE RICARICARE IL SUBFILE
036800970526     C*         1= AGGIORNAMENTI EFFETTUATI, OCCORRE RICARICARE
036900970526     C                   Z-ADD     0             SWMODI            1 0
037000970530     C*
037100970530     C* REIMPOSTA INDICATORI DI ERRORE
037200060313     C                   SETOFF                                       808281
037300970526     C*
037400060403     c
037500060403     C* CONTROLLA SE RICHIESTO NUOVO POSIZIONAMENTO
037600970527IF  1C     VIDPNA        IFNE      DEPNAR
037700970527     C     VIDPCA        ORNE      DEPCAP
037800060313     C     VIDPde        ORNE      DEPpde
037900970526     C                   EXSR      CHKPAR                                       *CONTROLLA PARAMETRI
038000970526IF  2C     ERRL00        IFEQ      '0'                                          *NO ERORI
038100970526     C                   Z-ADD     1             SWPOSI                         *NUOVO POSIZIONAMENT
038200970527     C                   MOVEL     VIDPNA        DEPNAR                         *DEPOSITI
038300970527     C                   MOVEL     VIDPCA        DEPCAP
038400060313     C                   MOVEL     VIDpde        DEPpde
038500970526E   2C                   ENDIF
038600970526E   1C                   ENDIF
038700970526     C*
038800970526     C                   ENDSR
038900970526     C*------------------------------------------------------------------------*
039000970526     C* SETFIL - POSIZIONAMENTO E PRIMA LETTURA RECORD
039100970526     C*------------------------------------------------------------------------*
039200970526     C     SETFIL        BEGSR
039300061009     c
039400061009     c                   if        apricursor='S'
039500061009     c                   exsr      CHIUSQL
039600061009     c                   clear                   apricursor        1
039700061009     c                   endif
039800061009     c
039900061009     c                   clear                   wrkgetlista
040000061003     c
040100061003     c* Se inserita la data recupero la versione
040200061003     c                   if        amgpde>0
040300061003     c                   clear                   tisie2ds
040400061003     c                   eval      sie2dri=amgpde
040500061003     c                   call      'TISIE2R'
040600061003     c                   parm                    tisie2ds
040700061009     c
040800061009     c                   move      sie2ver       vidver
040900061009     c                   eval      viddve=sie2des
041000061012     c                   movel     §15cod        wiso              2
041100061009     c
041200061003     c*
041300061006      *?Imposto la parte fissa dell'istruzione sql
041400061006     c                   eval      wrkgetlista =
041500061009     c                             'select * from dpcro00f where crover=' +
041600061009     c                             %editc(sie2ver:'Z')
041700061009     c                   else
041800061009     c                   clear                   vidver
041900061009     c                   clear                   viddve
042000061009     c*
042100061009     c                   eval      wrkgetlista =
042200061009     c                             'select * from dpcro00f where '
042300061009     c                   endif
042400061009     c
042500061009     c* Nazione
042600061009     c                   if        vidpna<>*blanks
042700061009     c                   eval      wrkgetlista = wrkgetlista +
042800061012     c                             ' and croiso2= ''' + wiso + ''''
042900061009     c                   endif
043000061009     c
043100061009     c* CAP
043200061009     c                   if        vidpca<>*blanks
043300061009     c                   eval      wrkgetlista = wrkgetlista +
043400061009     c                             ' and (substr(croptcb, 1, ' +
043500061009     c                             %editc(poscap:'Z') +
043600061009     c                             ') =''' + vidpca + '''' +
043700061009     c                             ' or substr(croptce, 1, ' +
043800061009     c                             %editc(poscap:'Z') +
043900061009     c                             ') =''' + vidpca + ''')'
044000061009     c
044100061006     c                   endif
044200061009     c* Se manca la data e quindi la vewrsione, devo abblancare il
044300061009     c*  primo AND impostato
044400061009     c                   if        amgpde=0
044500061009     c                   eval      %subst(wrkgetlista:31:3) ='   '
044600061009     c
044700061009      *?Ordinamento che cambia (senza la versione)
044800061009     c                   eval      wrkgetlista = wrkgetlista +
044900061009     c                             ' order by  croiso2, croptcb, crover'
045000061009     c                   else
045100061006      *?Ordinamento
045200061006     c                   eval      wrkgetlista = wrkgetlista +
045300061009     c                             ' order by crover, croiso2, croptcb'
045400061009     c                   endif
045500061006      *?Solo lettura
045600061006     c                   eval      wrkgetlista = wrkgetlista + ' for read only'
045700061009
045800061009     C/EXEC SQL
045900061009     C+ PREPARE s1 FROM :wrkgetlista
046000061009     C/END-EXEC
046100061009     C
046200061009     C/EXEC SQL
046300061009     C+ DECLARE A1 CURSOR FOR S1
046400061009     C/END-EXEC
046500061009     C
046600061009     C/EXEC SQL
046700061009     c+ OPEN A1
046800061009     C/END-EXEC
046900061003     c
047000970526     C                   EXSR      LETFIL
047100970526     C*
047200970526     C* IMPOSTA SWITCH DI FINE CARICAMENTO SFL
047300970526IF  1C     *IN99         IFEQ      *ON
047400970526     C                   Z-ADD     1             SFLEOF
047500970526E   1C                   ENDIF
047600970526     C*
047700970526     C                   ENDSR
047800970526     C*------------------------------------------------------------------------*
047900970526     C* LETFIL - LETTURA RECORD
048000970526     C*------------------------------------------------------------------------*
048100970526     C     LETFIL        BEGSR
048200061009     c* memorizzo che ho aperto il cursole del  sql
048300061009     c                   eval      apricursor='S'
048400970526     C*
048500970526     C* LEGGE FINO A:
048600970526     C*     - FINE FILE
048700970526     C*     - TROVATO RECORD VALIDO
048800970526     C                   MOVEL     'N'           WRECOK                         *RECORD VALIDO
048900970526DO  1C     *IN99         DOWEQ     *OFF
049000970526     C     WRECOK        ANDEQ     'N'
049100061009     C/EXEC SQL
049200061009     C+ FETCH NEXT FROM A1 INTO :dpcrods
049300061009     C/END-EXEC
049400061009
049500061009     c                   select
049600061009
049700061009     c                   when      sqlcod = 100
049800061009     c                   seton                                        99
049900061009     c                   when      sqlcod < 0
050000061009     c                   seton                                        99
050100061009     c                   endsl
050200970529     C*
050300970526     C* SE FINE FILE, IMPOSTA SWITCH DI FINE CARICAMENTO SFL
050400970526IF  2C     *IN99         IFEQ      *ON                                          *FINE FILE
050500970526     C                   Z-ADD     1             SFLEOF                         *FINE LETTURA
050600970530X   2C                   ELSE
050700970526     C                   EXSR      CHKREC                                       *CONTROLLA RECORD
050800970530E   2C                   ENDIF
050900970526E   1C                   ENDDO
051000061009     c
051100061009     C                   ENDSR
051200061009     C*------------------------------------------------------------------------*
051300061009     C* chiudo  l'SQL
051400061009     C*------------------------------------------------------------------------*
051500061009     C     CHIUSQL       BEGSR
051600061009     C/EXEC SQL
051700061009     c+ CLOSE A1
051800061009     C/END-EXEC
051900970526     C*
052000970526     C                   ENDSR
052100970526     C*------------------------------------------------------------------------*
052200970526     C* CHKREC - CONTROLLA VALIDITA' RECORD
052300970526     C*------------------------------------------------------------------------*
052400970526     C     CHKREC        BEGSR
052500060922     C                   MOVEL     'S'           WRECOK                         *VALIDITA' RECORD
052600060329     c
052700970526     C                   ENDSR
052800970526     C*------------------------------------------------------------------------*
052900970526     C* IRESFL - IMPOSTA I CAMPI DEL SFILE DAL FILE DI DATA BASE
053000970526     C*------------------------------------------------------------------------*
053100970526     C     IRESFL        BEGSR
053200970529     C*
053300970529     C                   MOVEL     *BLANKS       OPZKEY
053400061009     c                   movel     croiso2       v5ciso
053500061009     c                   movel     §15des        v5diso
053600061009     c                   movel     croptcb       v5cptcb
053700061009     c                   movel     croptce       v5cptce
053800061009     c                   movel     croddep       v5cddep
053900061009     c                   movel     crover        v5cver
054000061009     c* Se c'e' la data la versione è già decodificata sopra, altrimenti
054100061009     c*  la visualizzo su ogni riga
054200061009     c                   if        amgpde=0
054300061009     c     crover        chain     dpcve01l
054400061009     c                   if        %found(dpcve01l)
054500061009     c                   eval      v5dver=cvedes
054600061009     c                   else
054700061009     c                   eval      v5dver='- ???????'
054800061009     c                   endif
054900061009     c
055000061009     c                   else
055100061009     c                   clear                   v5dver
055200061009     c                   endif
055300060313     c
055400970526     C                   ENDSR
055500970526     C*------------------------------------------------------------------------*
055600970526     C* CHKPAR - CONTROLLA I PARAMETRI DI INIZIO RICERCA
055700970526     C*------------------------------------------------------------------------*
055800970526     C     CHKPAR        BEGSR
055900061009     c
056000060313     c* controllo data di riferimento se errata
056100060922     c                   clear                   amgpde
056200061009     C                   CLEAR                   VIDISO
056300061009     C                   CLEAR                   VIDDIS
056400060313     c                   if        vidpde>0
056500060313     C                   Z-ADD     vidpde        G08DAT
056600060313     C                   Z-ADD     *ZEROS        G08INV
056700060313     C                   MOVEL     '0'           G08ERR
056800060313     C                   CALL      'XSRDA8'
056900060313     C                   PARM                    WLBDA8
057000060313     c                   if        g08err='1'
057100060313     C                   MOVEL     MSG(3)        DSMSMS
057200060313     C                   MOVEL     '1'           ERRL00
057300060922     C                   SETON                                        81
057400061003     c                   goto      endchk
057500060313E   2C                   else
057600060314     c                   z-add     g08dat        vidpde
057700060922     C                   Z-ADD     G08INV        amgpde            8 0          *DATA CORRENTE AA/M/
057800060313     c                   endif
057900060313     c                   endif
058000061009     c* Nazione obbligatoria
058100061009     c                   if        vidpna=*blanks
058200061009     C                   MOVEL     MSG(5)        DSMSMS
058300061009     C                   MOVEL     '1'           ERRL00
058400061009     C                   SETON                                        82
058500061009     c                   goto      endchk
058600061009     c                   endif
058700061003     c                   clear                   ds15
058800061009     c* Controllo nazione
058900061003     c                   if        vidpna<>*Blanks
059000061003     c                   eval      kcod='15'
059100061003     c                   movel(p)  vidpna        kkey
059200061003     c     ktab          chain     tabel00f
059300061003     c                   if        not %found(tabel00f) or tblflg<>*blanks
059400061003     C                   MOVEL     MSG(4)        DSMSMS
059500061003     C                   MOVEL     '1'           ERRL00
059600061003     C                   SETON                                        82
059700061003     c                   goto      endchk
059800061003     c                   endif
059900061003     c                   movel     tbluni        ds15
060000061012     c                   movel     §15cod        vidiso
060100061009     c                   movel     §15des        viddis
060200061003     c                   endif
060300061006     c* Controllo se riempito il camp del cap per posizionamento
060400061006     c                   setoff                                       15
060500061009     c                   clear                   poscap            2 0
060600061006     c                   if        vidpca<>*blanks
060700061006     c     ' '           checkr    vidpca        poscap                   15
060800061006     c                   endif
060900970526     C*
061000061003     C     endchk        ENDSR
061100970527     C*------------------------------------------------------------------------*
061200970527     C* CARTAB - CARICA LE TABELLE OCCORRENTI
061300970527     C*------------------------------------------------------------------------*
061400970527     C     CARTAB        BEGSR
061500970527     C*
061600970527     C                   ENDSR
061700970526     C*------------------------------------------------------------------------*
061800970526     C* CHKFOP - CONTROLLA IL TIPO DI GESTIONE E IMPOSTA OPZIONI E FUNZIONI
061900970526     C*------------------------------------------------------------------------*
062000970526     C     CHKFOP        BEGSR
062100970626     C*
062200970626     C* SE L'UTENTE E' IN SCELTA, ABILITA SOLO QUELLA
062300060404IF  1C     opzione       IFEQ      '01'
062400970626     C                   MOVEL     OPD(1)        OPDRIG                         *SCELTA
062500970626     C                   MOVEL     'S'           OPDATT
062600970626     C                   MOVEL     OPDRIG        OPD(1)
062700970626X   1C                   ELSE
062800970626     C* OPZIONI
062900970626     C                   MOVEL     OPD(5)        OPDRIG                         *INTERROGAZIONE
063000970626     C                   MOVEL     'S'           OPDATT
063100970626     C                   MOVEL     OPDRIG        OPD(5)
063200970626E   1C                   ENDIF
063300970526     C*
063400970526     C* GESTIONE OPZIONI A VIDEO
063500970526     C                   Z-ADD     0             OPZGRU            5 0          *INDICE X OPZIONI
063600970526     C                   EXSR      OPZGES
063700970526     C*
063800970526     C* GESTIONE TASTI FUNZIONALI A VIDEO
063900970526     C                   Z-ADD     *ZEROS        FUNGRU            5 0          *INDICE X FUNZIONI
064000970526     C                   EXSR      FUNGES
064100970526     C*
064200970526     C                   ENDSR
064300020919     C*--------------------------------------------------------------------------------------------*
064400020919     C* REPERISCE I DATI UTENTE
064500020919     C*--------------------------------------------------------------------------------------------*
064600020919     C     REPDATIUTE    BEGSR
064700020919     C*
064800020919     C* INIZIALIZZA VARIABILI DI WRK
064900020919     C                   CLEAR                   TIBS34DS
065000020919     C                   CLEAR                   AZUTEDS
065100020919     C                   CLEAR                   DDATIUTE
065200020919     C*
065300020919     C     *DTAARA       DEFINE    §azute        azuteds
065400020919     C     *DTAARA       DEFINE    §datiute      ddatiute
065500020919     C                   IN(E)     *DTAARA
065600020919if  1C                   IF        %Error
065700020919     c                   EVAL      I34Tla = 'L'
065800020919     C                   CALL      'TIBS34R'
065900020919     C                   PARM                    Tibs34Ds
066000020919     C                   IN        *DTAARA
066100020919e   1C                   ENDIF
066200020919     C*
066300020919     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
066400020919     C                   MOVEL(P)  RSUT          DSFIRS           20
066500020919     C*
066600020919     C                   ENDSR
066700970526     C*------------------------------------------------------------------------*
066800970526     C* *INZSR - OPERAZIONI INIZIALI
066900970526     C*------------------------------------------------------------------------*
067000970526     C     *INZSR        BEGSR
067100970526     C*---
067200970526     C* RICEVIMENTO PARAMETRI
067300970526     C*---
067400970526     C     *ENTRY        PLIST
067500970612     C                   PARM                    KPJBA
067600061002     C                   PARM                    tiSIE8ds
067700970526     C*
067800970526     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
067900970526     C     XMSGPA        PLIST
068000970526     C                   PARM                    DSMSMS
068100970526     C                   PARM                    DSMSPG
068200970526     C                   PARM                    DSMSKE
068300970526     C                   PARM                    DSMSER
068400970526     C*
068500970526     C* IMPOSTA LA DS -TASTI PREMUTI-
068600970526     C                   CALL      'XKEY'
068700970526     C                   PARM                    DSKEY
068800970526     C*---
068900970526     C* VARIABILI NON RIFERITE AL DATA BASE
069000970526     C*---
069100970526     C     *LIKE         DEFINE    SFLNBR        SFLPAG
069200970526     C     *LIKE         DEFINE    SFLPAG        WFLPAG
069300970526     C     *LIKE         DEFINE    SFLNBR        SFLEOF
069400970526     C     *LIKE         DEFINE    SFLNBR        SFLERR
069500970526     C     *LIKE         DEFINE    SFLNBR        SFLULT
069600970526     C     *LIKE         DEFINE    SFLNBR        WFLULT
069700970526     C     *LIKE         DEFINE    SFLNBR        MSGULT
069800970526     C     *LIKE         DEFINE    SFLNBR        SFLRIG
069900970526     C     *LIKE         DEFINE    FUDFUN        FUNKEY
070000061009     C     *LIKE         DEFINE    isie8OP0      DEPOP0                         *DI DEPOSITO
070100061009     C     *LIKE         DEFINE    isie8op0      WOP0                           *DI LAVORO
070200061009     C     *LIKE         DEFINE    isie8op1      WOP1
070300061003     C     *LIKE         DEFINE    isie8nar      DNAR
070400061009     C     *LIKE         DEFINE    isie8cap      DCAP
070500061003     C     *LIKE         DEFINE    isie8dri      Ddde
070600061003     C     *LIKE         DEFINE    isie8nar      depnar
070700061009     C     *LIKE         DEFINE    isie8cap      depcap
070800061003     C     *LIKE         DEFINE    isie8dri      deppde
070900970526     C*---
071000970526     C* VARIABILI DI CONTROLLO
071100970526     C*---
071200970526     C                   MOVEL     '0'           FINL00            1            *FINE PGM
071300970526     C                   MOVEL     '0'           F03L00            1            *PREMUTO F03
071400970526     C                   MOVEL     '0'           F12L00            1            *PREMUTO F12
071500970526     C                   MOVEL     '0'           ERRL00            1            *ERRORE
071600970526     C                   MOVEL     'S'           WRECOK            1            *VALIDITA' RECORD
071700970530     C                   Z-ADD     12            SFLPAG                         *N°RIGHE X PAG
071800970526     C*
071900970526     C* IMPOSTA LA DS -INVIO MESSAGGI-
072000061002     C                   MOVEL     'TISIE8R'     DSMSPG
072100061002     C                   MOVEL     'TISIE8R'     MSGQUE
072200970526     C                   MOVEL     *BLANKS       MSGKEY
072300970526     C                   MOVEL     *BLANKS       DSMSMS
072400970526     C                   MOVEL     *BLANKS       DSMSKE
072500970526     C                   Z-ADD     *ZEROS        DSMSER
072600970526     C*---
072700970526     C* CHIAVI DI LETTURA
072800970526     C*---
072900060922     c     Ktab          Klist
073000060922     c                   Kfld                    codut             1 0
073100060922     c                   Kfld                    kCod
073200060922     c                   Kfld                    kKey
073300060922     c                   z-add     1             codut
073400060922     c
073500970527     C* REPERIMENTO DATI UTENTE
073600020919     C                   EXSR      REPDATIUTE
073700970527     C*
073800970527     C* CARICA LE TABELLE OCCORRENTI
073900970527     C                   EXSR      CARTAB
074000970526     C*
074100970526     C* DEPOSITA LE OPZIONI DI ENTRATA
074200061003     C                   MOVEL     isie8op0      DEPOP0                         *LIVELLO PROCEDURA
074300970526     C*
074400970526     C* CALCOLA LA DATA CORRENTE
074500970526     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
074600970526     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
074700970526     C                   Z-ADD     WN8           G08DAT
074800970526     C                   Z-ADD     *ZEROS        G08INV
074900970526     C                   MOVEL     '0'           G08ERR
075000970526     C                   CALL      'XSRDA8'
075100970526     C                   PARM                    WLBDA8
075200970526     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
075300060313     c
075400060313     c* Se non passata la data di ingresso, uso data corrente
075500061003     c                   if        isie8dri=0
075600060922     c                   eval      amgpde=datcor
075700060313     c                   eval      vidpde=wn8
075800060313     c                   else
075900061009     c                   eval      amgpde=isie8dri
076000060313     c* La imposto a video
076100061003     C                   Z-ADD     isie8dri      G08INV
076200060313     C                   Z-ADD     *ZEROS        G08DAT
076300060313     C                   MOVEL     '3'           G08ERR
076400060313     C                   CALL      'XSRDA8'
076500060313     C                   PARM                    WLBDA8
076600060313     C                   Z-ADD     G08DAT        VIDpde                         *DATA RIFERIMENTO
076700060313     c                   endif
076800970526     C*
076900970526     C* IMPOSTA I PARAMETRI DI INGRESSO
077000061003     C                   MOVEL     isie8nar      VIDPNA                         *NAZIONE
077100061003     C                   MOVEL     isie8cap      VIDPCA                         *CAP
077200060313     c
077300970526     C                   EXSR      CHKPAR
077400060922     c
077500970526IF  1C     ERRL00        IFEQ      '1'
077600061009     C                   MOVEL     '1'           osie8err
077700061009     C                   MOVEL     DSMSMS        osie8MSG
077800970526X   1C                   ELSE
077900970527     C                   MOVEL     VIDPNA        DEPNAR                         *DEPOSITI
078000970527     C                   MOVEL     VIDPCA        DEPCAP
078100060313     C                   MOVEL     VIDpde        DEPpde
078200970526E   1C                   ENDIF
078300970526     C*
078400970526     C* IMPOSTA LE OPZIONI E FUNZIONI A VIDEO
078500060404     c                   eval      opzione=%subst(depop0:2:2)
078600970526     C                   EXSR      CHKFOP
078700970526     C*
078800970530     C* IMPOSTA IL TIPO DI GESTIONE IN TESTATA
078900060404IF  1C     opzione       IFEQ      '01'
079000970530     C                   MOVEL     DEC(3)        VIDING
079100970530E   1C                   ENDIF
079200060404IF  1C     opzione       IFEQ      '02'
079300970530     C                   MOVEL     DEC(1)        VIDING
079400970530E   1C                   ENDIF
079500060404IF  1C     opzione       IFEQ      '05'
079600970530     C                   MOVEL     DEC(2)        VIDING
079700970530E   1C                   ENDIF
079800970526     C*
079900970526     C                   ENDSR
080000970526     C*------------------------------------------------------------------------*
080100970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
080200970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
080300970526     C*  LA PARTE DI SPECIFICHE 'C' SOTTOSTANTE SI RIFA' A PARTICOLARI
080400970526     C*  STANDARDS DI PROGRAMMAZIONE E NELLA MAGGIOR PARTE DEI CASI
080500970526     C*  NON E' NECESSARIO MODIFICARLA, NEL CASO FARE ATTENZIONE.
080600970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
080700970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
080800970526     C*------------------------------------------------------------------------*
080900970526     C* POSSFL - RICARICAMENTO SFL DA NUOVO POSIZIONAMENTO FILE DA CARICARE
081000970526     C*------------------------------------------------------------------------*
081100970526     C     POSSFL        BEGSR
081200970526     C*
081300970526     C* CLEAR SFL (ELIMINA CONTENUTO SFL)
081400970526     C                   EXSR      CLRSFL
081500970526     C*
081600970526     C* CONTROLLO SE VI SONO REC. CHE SODDISFANO I PARAMETRI IN INPUT
081700970526     C                   EXSR      SETFIL
081800970526     C*
081900970526     C* SE ESISTONO RECORD VALIDI CARICO IL SUBFILE
082000970526     C     SFLEOF        IFEQ      0
082100970526     C                   EXSR      CARSFL
082200970526     C                   END
082300970526     C*
082400970526     C                   ENDSR
082500970526     C*------------------------------------------------------------------------*
082600970526     C* ENTATT - GESTIONE SFL CON TASTO FUNZIONALE ENTER
082700970526     C*------------------------------------------------------------------------*
082800970526     C     ENTATT        BEGSR
082900970526     C*
083000970526     C* AZZERO SWITCH DI ERRORI IN RECORD SFL (OPZOK: 1=ERRORE 0=OK)
083100970526     C                   Z-ADD     0             OPZOK             1 0
083200970526     C*
083300970526     C* AZZERO CONTATORE DI RIGHE SFL VARIATE
083400970526     C                   Z-ADD     0             SFLVAR            5 0
083500970526     C*
083600970526     C* INIZIALIZZO COMODO CON PRIMO RECORD SFL IN ERRORE
083700970526     C                   Z-ADD     0             SFLERR
083800970526     C*
083900970526     C* SALVO ULTIMO RECORD SCRITTO IN SFL IN COMODO
084000970526     C                   Z-ADD     SFLULT        WFLULT
084100970526     C*
084200970526     C* SCORRIMENTO DEI SOLI RECORD VARIATI NEL SFL
084300061002     C                   READC     SIE8D05                                98
084400970526     C     *IN98         DOWEQ     '0'
084500970526     C*
084600970526     C* ADD CONTATORE DI RIGHE SFL VARIATE
084700970526     C                   ADD       1             SFLVAR
084800970526     C*
084900970526     C* SE RIGHE PRECEDENTI SENZA ERRORI (OPZOK: 1=ERRORE 0=OK)
085000970526     C     OPZOK         IFEQ      0
085100970526     C*
085200970526     C* CONTROLLO ED ESECUZIONE OPZIONI RICHIESTE (OPZOK: 1=ERRORE 0=OK)
085300970526     C     OPZKEY        IFNE      *BLANKS
085400970526     C                   EXSR      OPZCON
085500970526     C                   END
085600970526     C*
085700970526     C* SE OK    : 1. AZZERO SFLNXTCHG PER RECORD SFL
085800970526     C*            2. PULIZIA SCELTA IN SFL
085900970526     C     OPZOK         IFEQ      0
086000970526     C                   MOVEL     '0'           *IN41
086100970526     C                   MOVEL     *BLANKS       OPZKEY
086200970526     C                   ELSE
086300970526     C*
086400970526     C* SE NON OK: 1. ACCENDO SFLNXTCHG COSI' COSTRINGO ALLA CORREZIONE
086500970526     C*            2. IMPOSTO RECORD CON PRIMO ERRORE
086600970526     C                   MOVEL     '1'           *IN41
086700970526     C                   Z-ADD     SFLULT        SFLERR
086800970526     C                   END
086900970526     C                   ELSE
087000970526     C*
087100970526     C* SE RIGHE PRECEDENTI CON ERRORI (SFLERR DIVERSO DA 0):
087200970526     C*   . ACCENDO SFLNXTCHG COSI' RIPROPONE LA RIGA IL PROSSIMO GIRO
087300970526     C                   MOVEL     '1'           *IN41
087400970526     C                   END                                                    OPZOK=0
087500970526     C*
087600970526     C* IMPOSTO RIGA SFL SUL QUALE POSIZIONARSI:
087700970526     C*   . SE ERRORE        MI POSIZIONO SULLA PRIMA RIGA IN ERRORE
087800970526     C*   . SE NESSUN ERRORE MI POSIZIONO SULL'ULTIMA RIGA VARIATA
087900970526     C     OPZOK         IFNE      0
088000970526     C                   Z-ADD     SFLERR        SFLNBR
088100970526     C                   ELSE
088200970526     C                   Z-ADD     SFLULT        SFLNBR
088300970526     C                   END
088400970526     C*
088500970526     C* AGGIORNO RECORD SFL
088600061002     C                   UPDATE    SIE8D05
088700970526     C*
088800061002     C                   READC     SIE8D05                                98
088900970526     C                   END                                                    DO WHILE
089000970526     C*
089100970526     C* REIMPOSTO ULTIMO RECORD SCRITTO IN SFL DA COMODO
089200970526     C                   Z-ADD     WFLULT        SFLULT
089300970526     C*
089400970526     C* SE NESSUNA RIGA VARIATA, CON ENTER SI RITORNA AL PGM CHIAMANTE
089500970526     C     SFLVAR        IFEQ      0
089600970526     C*---                 MOVEL'1'       FINL00
089700970526     C                   END
089800970526     C*
089900970526     C                   ENDSR
090000970526     C*------------------------------------------------------------------------*
090100970526     C* CLRSFL - ELIMINAZIONE CONTENUTO SFL
090200970526     C*------------------------------------------------------------------------*
090300970526     C     CLRSFL        BEGSR
090400970526     C*
090500970526     C* INIZIALIZZO SWITCH DI FINE FILE "FISICO"
090600970526     C* (0=NON FINE FILE  1=FINE FILE)
090700970526     C                   Z-ADD     0             SFLEOF
090800970526     C*
090900970526     C* INIZIALIZZO CONTATORI DI ULTIMO RECORD IN SFL
091000970526     C                   Z-ADD     0             SFLULT
091100970526     C*
091200970526     C* ACCENSIONE INDICATORE PER CLEAR SFL
091300970526     C                   MOVEL     '1'           *IN40
091400061002     C                   WRITE     SIE8D06
091500970526     C                   MOVEL     '0'           *IN40
091600970526     C*
091700970526     C                   ENDSR
091800970526     C*------------------------------------------------------------------------*
091900970526     C* CARSFL - CARICAMENTO SFL
092000970526     C*------------------------------------------------------------------------*
092100970526     C     CARSFL        BEGSR
092200970526     C*
092300970526     C* SE IL SFL E' VUOTO INIZIO LETTURA DA POSIZ. RICHIESTA
092400970526     C     SFLULT        IFEQ      0
092500970526     C                   EXSR      SETFIL
092600970526     C                   END
092700970526     C*
092800970526     C* SALVO SFLRCDNBR PER CONFRONTI SUCCESSIVI
092900970526     C                   Z-ADD     SFLULT        WFLULT
093000970526     C*
093100970526     C* CICLO:
093200970526     C*    -CARICO NUMERO DI RIGHE=SFLPAG
093300970526     C*    -SFLEOF=0 (NON FINE FILE "FISICO")
093400970526     C                   Z-ADD     1             SFLRIG
093500970526     C     SFLRIG        DOWLE     SFLPAG
093600970526     C     SFLEOF        ANDEQ     0
093700970526     C*
093800970526     C* IMPOSTO CAMPI DEL RECORD SFL SOLO SE RECORD E' OK CIOE' ...
093900970526     C                   MOVEL     *BLANKS       OPZKEY
094000970526     C*
094100970526     C* IMPOSTA I CAMPI DEL RECORD DEL SFL DA FILE DA CARICARE
094200970526     C                   EXSR      IRESFL
094300970526     C*
094400970526     C* AGGIORNA CHIAVE PER RECORD SFL E SFLRCDNBR
094500970526     C                   ADD       1             SFLULT
094600970526     C*
094700970526     C* SCRITTURA RECORD SFL
094800061002     C                   WRITE     SIE8D05
094900970526     C                   ADD       1             SFLRIG
095000970526     C*
095100970526     C* LETTURA FILE DA CARICARE
095200970526     C                   EXSR      LETFIL
095300970526     C                   END                                                    DO WHILE
095400970526     C*
095500970526     C* IMPOSTO SFLRCDNBR DELLA SUCCESSIVA PAGINA SE CARICATI ALTRI RECORD
095600970526     C     WFLULT        IFNE      SFLULT
095700970526     C                   Z-ADD     WFLULT        SFLNBR
095800970526     C                   ADD       1             SFLNBR
095900970526     C                   END
096000970526     C*
096100970526     C                   ENDSR
096200970526     C*------------------------------------------------------------------------*
096300970526     C* OUTSFL - EMISSIONE SUBFILE A VIDEO
096400970526     C*------------------------------------------------------------------------*
096500970526     C     OUTSFL        BEGSR
096600970526     C*
096700970526     C* WRITE RECORD TESTATA VIDEO
096800061002     C                   WRITE     SIE8D01
096900970526     C*
097000970526     C* VISUALIZZAZIONE MESSAGGIO
097100970526     C                   EXSR      SNDMSG
097200970526     C*
097300970526     C* WRITE RECORD OPZIONI VIDEO
097400061002     C                   WRITE     SIE8D02
097500970526     C*
097600970526     C* WRITE RECORD PER POSIZIONAMEMTO VIDEO
097700061002     C                   WRITE     SIE8D03
097800970526     C*
097900970526     C* WRITE RECORD CON DESCRIZIONI COLONNE SFL
098000061002     C                   WRITE     SIE8D04
098100970526     C*
098200970526     C* GESTIONE AUTOMATICA SFLEND (+ A VIDEO)
098300970526     C* SFLEOF=0 NON RAGGIUNTO FINE FILE "FISICO"
098400970526     C     SFLEOF        IFEQ      0
098500970526     C                   MOVEL     '0'           *IN42
098600970526     C                   ELSE
098700970526     C*
098800970526     C* SFLEOF=1 RAGGIUNTO FINE FILE "FISICO"
098900970526     C                   MOVEL     '1'           *IN42
099000970526     C                   END
099100970526     C*
099200970526     C* WRITE RECORD TASTI FUNZIONALI
099300061002     C                   WRITE     SIE8D09
099400970526     C*
099500970526     C* CASO DI SFL VUOTO
099600970526     C     SFLULT        IFLE      0
099700970526     C*
099800970526     C*    - WRITE RECORD VUOTO
099900061002     C                   WRITE     SIE8D0A
100000061002     C                   READ      SIE8D0A                                99
100100970526     C*
100200970526     C                   ELSE
100300970526     C*
100400970526     C* CASO DI SFL CON RECORDS
100500970526     C*
100600970526     C*    - WRITE RECORD CONTROLLO SFL
100700061002     C                   WRITE     SIE8D06
100800061002     C                   READ      SIE8D06                                99
100900970526     C                   END
101000970526     C*
101100970526     C* READ RECORD PER POSIZIONAMEMTO VIDEO
101200061002     C                   READ      SIE8D03                                99
101300970526     C*
101400970526     C                   ENDSR
101500970526     C*------------------------------------------------------------------------*
101600970526     C* SNDMSG - MANDA MESSAGGIO
101700970526     C*------------------------------------------------------------------------*
101800970526     C     SNDMSG        BEGSR
101900970526     C*
102000970526     C* PULIZIA SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
102100970526     C                   MOVEL     '1'           *IN40
102200061002     C                   WRITE     SIE8DMC
102300970526     C                   MOVEL     '0'           *IN40
102400970526     C*
102500970526     C* VISUALIZZAZIONE MESSAGGIO
102600970526     C                   Z-ADD     0             MSGULT
102700970526     C                   CALL      'XMSG'        XMSGPA
102800970526     C     DSMSER        IFEQ      0
102900970526     C                   MOVEL     DSMSKE        MSGKEY
103000970526     C                   END
103100970526     C                   Z-ADD     1             MSGULT
103200061002     C                   WRITE     SIE8DMS                                      SFL MESS
103300061002     C                   WRITE     SIE8DMC                                      CONTR MESS
103400970526     C*
103500970526     C* MANDO MESSAGGIO *BLANKS PER PULIRE SFLMSG
103600970526     C                   MOVEL     *BLANKS       DSMSMS
103700970526     C*
103800970526     C                   ENDSR
103900970526     C*------------------------------------------------------------------------*
104000970526     C* FUNCON - GESTIONE SCELTE DELL'OPERATORE A VIDEO
104100970526     C*------------------------------------------------------------------------*
104200970526     C     FUNCON        BEGSR
104300970526     C*
104400970526     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
104500970526     C                   EXSR      FUNVAR
104600970526     C*
104700970526     C* PULISCO CAMPI DI LAVORO
104800970526     C                   Z-ADD     0             I                 5 0
104900970526     C                   Z-ADD     0             W                 5 0
105000970526     C*
105100970526     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
105200970526     C     FUNKEY        IFNE      *BLANKS
105300970526     C     1             DO        24            I
105400970526     C                   MOVEL     FUD(I)        FUDRIG
105500970526     C*
105600970526     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
105700970526     C     FUNKEY        IFEQ      FUDFUN
105800970526     C     FUDATT        ANDEQ     'S'
105900970526     C                   Z-ADD     I             W
106000970526     C                   END
106100970526     C                   END                                                    DO WHILE
106200970526     C*
106300970526     C* FUNZIONE NON ATTIVA (ERRORE)
106400970526     C     W             IFEQ      0
106500970526     C*
106600970526     C* EMISSIONE MESSAGGIO DI ERRORE
106700970526     C                   MOVEL     MSG(2)        DSMSMS
106800970526     C                   END                                                    W=0
106900970526     C                   END                                                    FUNKEY NE *BLANKS
107000970526     C*
107100970526     C* FUNZIONE ATTIVA (PROCEDI ...)
107200970526     C     W             IFNE      0
107300970526     C     FUNKEY        OREQ      *BLANKS
107400970526     C*
107500970526     C     FUNKEY        IFEQ      *BLANKS
107600970526     C                   EXSR      ENTGES
107700970526     C                   END
107800970526     C     FUNKEY        IFEQ      '03'
107900970526     C                   EXSR      F03GES
108000970526     C                   END
108100970526     C     FUNKEY        IFEQ      '05'
108200970526     C                   EXSR      F05GES
108300970526     C                   END
108400970526     C     FUNKEY        IFEQ      '06'
108500970526     C                   EXSR      F06GES
108600970526     C                   END
108700060926     C     FUNKEY        IFEQ      '07'
108800060926     C                   EXSR      F07GES
108900060926     C                   END
109000970526     C     FUNKEY        IFEQ      '12'
109100970526     C                   EXSR      F12GES
109200970526     C                   END
109300970529     C     FUNKEY        IFEQ      '13'
109400970529     C                   EXSR      F13GES
109500970529     C                   END
109600970526     C     FUNKEY        IFEQ      '23'
109700970526     C                   EXSR      F23GES
109800970526     C                   END
109900970526     C     FUNKEY        IFEQ      '24'
110000970526     C                   EXSR      F24GES
110100970526     C                   END
110200970526     C     FUNKEY        IFEQ      'RU'
110300970526     C                   EXSR      RUPGES
110400970526     C                   END
110500970526     C*
110600970526     C                   END
110700970526     C*
110800970526     C                   ENDSR
110900970526     C*------------------------------------------------------------------------*
111000970526     C* FUNVAR -TRADUCE INDICATORI DI TASTO FUNZIONALE IN VARIABILE
111100970526     C*------------------------------------------------------------------------*
111200970526     C     FUNVAR        BEGSR
111300970526     C*
111400970526     C* FUNKEY=*BLANKS SIGNIFICA ENTER
111500970526     C     KEY           IFEQ      ENTER
111600970526     C                   MOVEL     *BLANKS       FUNKEY
111700970526     C                   END
111800970526     C     KEY           IFEQ      F1
111900970526     C                   MOVEL     '01'          FUNKEY
112000970526     C                   END
112100970526     C     KEY           IFEQ      F2
112200970526     C                   MOVEL     '02'          FUNKEY
112300970526     C                   END
112400970526     C     KEY           IFEQ      F3
112500970526     C                   MOVEL     '03'          FUNKEY
112600970526     C                   END
112700970526     C     KEY           IFEQ      F4
112800970526     C                   MOVEL     '04'          FUNKEY
112900970526     C                   END
113000970526     C     KEY           IFEQ      F5
113100970526     C                   MOVEL     '05'          FUNKEY
113200970526     C                   END
113300970526     C     KEY           IFEQ      F6
113400970526     C                   MOVEL     '06'          FUNKEY
113500970526     C                   END
113600970526     C     KEY           IFEQ      F7
113700970526     C                   MOVEL     '07'          FUNKEY
113800970526     C                   END
113900970526     C     KEY           IFEQ      F8
114000970526     C                   MOVEL     '08'          FUNKEY
114100970526     C                   END
114200970526     C     KEY           IFEQ      F9
114300970526     C                   MOVEL     '09'          FUNKEY
114400970526     C                   END
114500970526     C     KEY           IFEQ      F10
114600970526     C                   MOVEL     '10'          FUNKEY
114700970526     C                   END
114800970526     C     KEY           IFEQ      F11
114900970526     C                   MOVEL     '11'          FUNKEY
115000970526     C                   END
115100970526     C     KEY           IFEQ      F12
115200970526     C                   MOVEL     '12'          FUNKEY
115300970526     C                   END
115400970526     C     KEY           IFEQ      F13
115500970526     C                   MOVEL     '13'          FUNKEY
115600970526     C                   END
115700970526     C     KEY           IFEQ      F14
115800970526     C                   MOVEL     '14'          FUNKEY
115900970526     C                   END
116000970526     C     KEY           IFEQ      F15
116100970526     C                   MOVEL     '15'          FUNKEY
116200970526     C                   END
116300970526     C     KEY           IFEQ      F16
116400970526     C                   MOVEL     '16'          FUNKEY
116500970526     C                   END
116600970526     C     KEY           IFEQ      F17
116700970526     C                   MOVEL     '17'          FUNKEY
116800970526     C                   END
116900970526     C     KEY           IFEQ      F18
117000970526     C                   MOVEL     '18'          FUNKEY
117100970526     C                   END
117200970526     C     KEY           IFEQ      F19
117300970526     C                   MOVEL     '19'          FUNKEY
117400970526     C                   END
117500970526     C     KEY           IFEQ      F20
117600970526     C                   MOVEL     '20'          FUNKEY
117700970526     C                   END
117800970526     C     KEY           IFEQ      F21
117900970526     C                   MOVEL     '21'          FUNKEY
118000970526     C                   END
118100970526     C     KEY           IFEQ      F22
118200970526     C                   MOVEL     '22'          FUNKEY
118300970526     C                   END
118400970526     C     KEY           IFEQ      F23
118500970526     C                   MOVEL     '23'          FUNKEY
118600970526     C                   END
118700970526     C     KEY           IFEQ      F24
118800970526     C                   MOVEL     '24'          FUNKEY
118900970526     C                   END
119000970526     C     KEY           IFEQ      ROLLUP
119100970526     C                   MOVEL     'RU'          FUNKEY
119200970526     C                   END
119300970526     C     KEY           IFEQ      HELP
119400970526     C                   MOVEL     'HP'          FUNKEY
119500970526     C                   END
119600970526     C*
119700970526     C                   ENDSR
119800970526     C*------------------------------------------------------------------------*
119900970526     C* FUNGES - GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI GESTITI DA PGM
120000970526     C*------------------------------------------------------------------------*
120100970526     C     FUNGES        BEGSR
120200970526     C*
120300970526     C* PULISCO CAMPI DI LAVORO
120400970526     C                   Z-ADD     0             W                 5 0
120500970526     C*
120600970526     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
120700970526     C* E CARICO TABELLA FUNZIONI ATTIVE
120800970526     C     1             DO        24            I
120900970526     C                   MOVEL     FUD(I)        FUDRIG
121000970526     C*
121100970526     C* PULIZIA FUNZIONI PRECEDENTI
121200970526     C                   MOVEL     *BLANKS       FUV(I)
121300970526     C*
121400970526     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
121500970526     C* (SALTA F23 E F24)
121600970526     C     FUDATT        IFEQ      'S'
121700970526     C     FUDFUN        ANDNE     '23'
121800970526     C     FUDFUN        ANDNE     '24'
121900970526     C                   ADD       1             W
122000970526     C                   MOVEL     FUDDES        FUV(W)
122100970526     C                   END
122200970526     C                   END                                                    DO WHILE
122300970526     C*
122400970526     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
122500970526     C* ALTRIMENTI DISATTIVO F24
122600970526     C     1             DO        24            I
122700970526     C                   MOVEL     FUD(I)        FUDRIG
122800970526     C*
122900970526     C     FUDFUN        IFEQ      '24'
123000970526     C     W             IFGT      6
123100970526     C                   MOVEL     'S'           FUDATT
123200970526     C                   ELSE
123300970526     C                   MOVEL     *BLANKS       FUDATT
123400970526     C                   END                                                    W GT 8
123500970526     C                   MOVEL     FUDRIG        FUD(I)
123600970526     C                   END
123700970526     C                   END                                                    DO WHILE
123800970526     C*
123900970526     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
124000970526     C     W             DIV       6             I
124100970526     C                   MVR                     Z
124200970526     C*
124300970526     C     Z             IFNE      0
124400970526     C     I             ADD       1             Z
124500970526     C                   ELSE
124600970526     C                   Z-ADD     I             Z
124700970526     C                   END
124800970526     C*
124900970526     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
125000970526     C                   ADD       1             FUNGRU
125100970526     C     FUNGRU        IFGT      Z
125200970526     C     FUNGRU        ORLT      1
125300970526     C                   Z-ADD     1             FUNGRU
125400970526     C                   END
125500970526     C*
125600970526     C* PRIMO GRUPPO DI 6 FUNZIONI
125700970526     C     FUNGRU        IFEQ      1
125800970526     C                   MOVEL     FUV(1)        FUN001
125900970526     C                   MOVEL     FUV(2)        FUN002
126000970526     C                   MOVEL     FUV(3)        FUN003
126100970526     C                   MOVEL     FUV(4)        FUN004
126200970526     C                   MOVEL     FUV(5)        FUN005
126300970526     C                   MOVEL     FUV(6)        FUN006
126400970526     C                   END
126500970526     C*
126600970526     C* SECONDO GRUPPO DI 6 FUNZIONI
126700970526     C     FUNGRU        IFEQ      2
126800970526     C                   MOVEL     FUV(7)        FUN001
126900970526     C                   MOVEL     FUV(8)        FUN002
127000970526     C                   MOVEL     FUV(9)        FUN003
127100970526     C                   MOVEL     FUV(10)       FUN004
127200970526     C                   MOVEL     FUV(11)       FUN005
127300970526     C                   MOVEL     FUV(12)       FUN006
127400970526     C                   END
127500970526     C*
127600970526     C* TERZO GRUPPO DI 6 FUNZIONI
127700970526     C     FUNGRU        IFEQ      3
127800970526     C                   MOVEL     FUV(13)       FUN001
127900970526     C                   MOVEL     FUV(14)       FUN002
128000970526     C                   MOVEL     FUV(15)       FUN003
128100970526     C                   MOVEL     FUV(16)       FUN004
128200970526     C                   MOVEL     FUV(17)       FUN005
128300970526     C                   MOVEL     FUV(18)       FUN006
128400970526     C                   END
128500970526     C*
128600970526     C* QUARTO GRUPPO DI 6 FUNZIONI
128700970526     C     FUNGRU        IFEQ      4
128800970526     C                   MOVEL     FUV(19)       FUN001
128900970526     C                   MOVEL     FUV(20)       FUN002
129000970526     C                   MOVEL     FUV(21)       FUN003
129100970526     C                   MOVEL     FUV(22)       FUN004
129200970526     C                   MOVEL     *BLANKS       FUN005
129300970526     C                   MOVEL     *BLANKS       FUN006
129400970526     C                   END
129500970526     C*
129600970526     C* F24 E F23 SE VI SONO, VENGONO SEMPRE VISUALIZZATI
129700970526     C                   MOVEL     *BLANKS       FUN007
129800970526     C                   MOVEL     *BLANKS       FUN008
129900970526     C*
130000970526     C     1             DO        24            I
130100970526     C                   MOVEL     FUD(I)        FUDRIG
130200970526     C     FUDFUN        IFEQ      '23'
130300970526     C     FUDATT        ANDEQ     'S'
130400970526     C                   MOVEL     FUDDES        FUN007
130500970526     C                   END
130600970526     C     FUDFUN        IFEQ      '24'
130700970526     C     FUDATT        ANDEQ     'S'
130800970526     C                   MOVEL     FUDDES        FUN008
130900970526     C                   END
131000970526     C                   END                                                    DO
131100970526     C*
131200970526     C                   ENDSR
131300970526     C*------------------------------------------------------------------------*
131400970526     C* OPZGES - GESTIONE VISUALIZZAZIONE OPZIONI GESTITE DAL PGM
131500970526     C*------------------------------------------------------------------------*
131600970526     C     OPZGES        BEGSR
131700970526     C*
131800970526     C* PULISCO CAMPI DI LAVORO
131900970526     C                   Z-ADD     0             W                 5 0
132000970526     C*
132100970526     C* RICERCO QUANTE OPZIONI SONO ATTIVE
132200970526     C* E CARICO TABELLA OPZIONI ATTIVE
132300970526     C     1             DO        24            I
132400970526     C                   MOVEL     OPD(I)        OPDRIG
132500970526     C*
132600970526     C* PULIZIA OPZIONI PRECEDENTI
132700970526     C                   MOVEL     *BLANKS       OPV(I)
132800970526     C*
132900970526     C* IMPOSTA OPZIONE ATTIVA IN TABELLA
133000970526     C     OPDATT        IFEQ      'S'
133100970526     C                   ADD       1             W
133200970526     C                   MOVEL     OPDDES        OPV(W)
133300970526     C                   END
133400970526     C                   END                                                    DO WHILE
133500970526     C*
133600970526     C* SE VI SONO PIU' DI 8 OPZIONI ATTIVE, ATTIVO FUNZIONE F23
133700970526     C* ALTRIMENTI DISATTIVO F23
133800970526     C     1             DO        24            I
133900970526     C                   MOVEL     FUD(I)        FUDRIG
134000970526     C*
134100970526     C     FUDFUN        IFEQ      '23'
134200970526     C     W             IFGT      8
134300970526     C                   MOVEL     'S'           FUDATT
134400970526     C                   ELSE
134500970526     C                   MOVEL     *BLANKS       FUDATT
134600970526     C                   END                                                    W GT 8
134700970526     C                   MOVEL     FUDRIG        FUD(I)
134800970526     C                   END
134900970526     C                   END                                                    DO WHILE
135000970526     C*
135100970526     C* IMPOSTO QUANTI GRUPPI DI 8 OPZIONI SI POSSONO VISUALIZZARE
135200970526     C     W             DIV       8             I
135300970526     C                   MVR                     Z                 5 0
135400970526     C*
135500970526     C     Z             IFNE      0
135600970526     C     I             ADD       1             Z
135700970526     C                   ELSE
135800970526     C                   Z-ADD     I             Z
135900970526     C                   END
136000970526     C*
136100970526     C* IMPOSTO SUCCESSIVE 8 OPZIONI ATTIVE A VIDEO
136200970526     C                   ADD       1             OPZGRU
136300970526     C     OPZGRU        IFGT      Z
136400970526     C     OPZGRU        ORLT      1
136500970526     C                   Z-ADD     1             OPZGRU
136600970526     C                   END
136700970526     C*
136800970526     C* PRIMO GRUPPO DI 8 OPZIONI
136900970526     C     OPZGRU        IFEQ      1
137000970526     C                   MOVEL     OPV(1)        OPZ001
137100970526     C                   MOVEL     OPV(2)        OPZ002
137200970526     C                   MOVEL     OPV(3)        OPZ003
137300970526     C                   MOVEL     OPV(4)        OPZ004
137400970526     C                   MOVEL     OPV(5)        OPZ005
137500970526     C                   MOVEL     OPV(6)        OPZ006
137600970526     C                   MOVEL     OPV(7)        OPZ007
137700970526     C                   MOVEL     OPV(8)        OPZ008
137800970526     C                   END
137900970526     C*
138000970526     C* SECONDO GRUPPO DI 8 OPZIONI
138100970526     C     OPZGRU        IFEQ      2
138200970526     C                   MOVEL     OPV(9)        OPZ001
138300970526     C                   MOVEL     OPV(10)       OPZ002
138400970526     C                   MOVEL     OPV(11)       OPZ003
138500970526     C                   MOVEL     OPV(12)       OPZ004
138600970526     C                   MOVEL     OPV(13)       OPZ005
138700970526     C                   MOVEL     OPV(14)       OPZ006
138800970526     C                   MOVEL     OPV(15)       OPZ007
138900970526     C                   MOVEL     OPV(16)       OPZ008
139000970526     C                   END
139100970526     C*
139200970526     C* TERZO GRUPPO DI 8 OPZIONI
139300970526     C     OPZGRU        IFEQ      3
139400970526     C                   MOVEL     OPV(17)       OPZ001
139500970526     C                   MOVEL     OPV(18)       OPZ002
139600970526     C                   MOVEL     OPV(19)       OPZ003
139700970526     C                   MOVEL     OPV(20)       OPZ004
139800970526     C                   MOVEL     OPV(21)       OPZ005
139900970526     C                   MOVEL     OPV(22)       OPZ006
140000970526     C                   MOVEL     OPV(23)       OPZ007
140100970526     C                   MOVEL     OPV(24)       OPZ008
140200970526     C                   END
140300970526     C*
140400970526     C                   ENDSR
140500970526     C*------------------------------------------------------------------------*
140600970526     C*  OPZCON - CONTROLLO ED ESECUZIONE OPZIONI SCELTE
140700970526     C*------------------------------------------------------------------------*
140800970526     C     OPZCON        BEGSR
140900970526     C*
141000970526     C* PULISCO CAMPI DI LAVORO
141100970526     C                   Z-ADD     0             I                 5 0
141200970526     C                   Z-ADD     0             W                 5 0
141300970526     C*
141400970526     C* RICERCA OPZIONE SCELTA IN TABELLA OPZIONI
141500970526     C     1             DO        24            I
141600970526     C                   MOVEL     OPD(I)        OPDRIG
141700970526     C*
141800970526     C* SALVO INDICE DI OPZIONE TROVATA ED ATTIVA (W)
141900970526     C     OPZKEY        IFEQ      OPDOPZ
142000970526     C     OPZKEY        OREQ      OPDOP1
142100970526     C*
142200970526     C     OPDATT        IFEQ      'S'
142300970526     C                   MOVEL     OPDOPZ        OPZKEY
142400970526     C                   Z-ADD     I             W
142500970526     C                   END
142600970526     C                   END
142700970526     C*
142800970526     C                   END                                                    DO WHILE
142900970526     C*
143000970526     C* OPZIONE NON ATTIVA (ERRORE)
143100970526     C     W             IFEQ      0
143200970526     C*
143300970526     C* OPZIONE NON OK
143400970526     C                   Z-ADD     1             OPZOK
143500970526     C*
143600970526     C* EMISSIONE MESSAGGIO DI ERRORE
143700970526     C                   MOVEL     MSG(1)        DSMSMS
143800970526     C                   ELSE
143900970526     C*
144000970526     C* OPZIONE ATTIVA (PROCEDI ...)
144100970526     C*
144200970526     C     OPZKEY        IFEQ      ' 1'
144300970526     C                   EXSR      O01GES
144400970526     C                   END
144500970526     C     OPZKEY        IFEQ      ' 2'
144600060328     C                   EXSR      O02GES
144700970526     C                   END
144800970611     C     OPZKEY        IFEQ      ' 3'
144900970611     C                   EXSR      O03GES
145000970611     C                   END
145100970526     C     OPZKEY        IFEQ      ' 4'
145200970526     C                   EXSR      O04GES
145300970526     C                   END
145400970526     C     OPZKEY        IFEQ      ' 5'
145500970526     C                   EXSR      O05GES
145600970526     C                   END
145700970526     C                   END                                                    OPZ.NON ATTIVA
145800970526     C*
145900970526     C                   ENDSR
146000970526     C*------------------------------------------------------------------------*
146100970526     C* NOTE COSTRUZIONE SCHIERE
146200970526     C*------------------------------------------------------------------------*
146300970526     C*
146400970526     C* -MSG-MESSAGGI
146500970526     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
146600970526     C* (NON TOCCARE MSG,1 E MSG,2)
146700970526     C*
146800970526     C* -CMD-COMANDI
146900970526     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
147000970526     C*
147100970526     C* -OPD-OPZIONI
147200970526     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
147300970526     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
147400970526     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
147500970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
147600970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
147700970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
147800970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
147900970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
148000970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
148100970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
148200970526     C*
148300970526     C* -FUD-FUNZIONI
148400970526     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
148500970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
148600970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
148700970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
148800970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
148900970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
149000970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
149100970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
149200970526     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
149300970526     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
149400970526     O*------------------------------------------------------------------------*
149500970526** DEC - DECODIFICHE
149600970526    GESTIONE                                                     1
149700970530INTERROGAZIONE                                                   2
149800970530     SCELTA                                                      3
1499009705264
1500009705265
150100970526** MSG - MESSAGGI
150200061002TISIE8R- Opzione non gestita                                                   1
150300061002TISIE8R- Tasto funzionale non gestito                                          2
150400061002TISIE8R- Data di riferimento errata
150500061009TISIE8R- Nazione inesistente                                                   4
150600061009TISIE8R- Per la selezione impostare almeno la nazione                          5
150700061002TISIE8R- Accettato solo un parametro di inizio lista (P.O. o Naz/CAP)          6
150800061011TISIE8R- CAP DPD non "trasformabile" in CAP Bartolini                          7
150900061002TISIE8R- 8                                                                     8
151000061002TISIE8R- 9                                                                     9
151100061002TISIE8R- 10                                                                   10
151200061002TISIE8R- 11                                                                   11
151300061002TISIE8R- 12                                                                   12
151400061002TISIE8R- 13                                                                   13
151500061002TISIE8R- 14                                                                   14
151600061002TISIE8R- 15                                                                   15
151700061002TISIE8R- 16                                                                   16
151800061002TISIE8R- 17                                                                   17
151900061002TISIE8R- 18                                                                   18
152000061002TISIE8R- 19                                                                   19
152100061002TISIE8R- 20                                                                   20
152200970526** OPD - OPZIONI
152300970526 1=Scelta          -1 -     (S/N)=N                        opz. 01¬
152400061009                   -2 -     (S/N)=N                        opz. 02¬
152500061009                   -3 -     (S/N)=N                        opz. 03¬
152600061009                   -4 -     (S/N)=N                        opz. 04¬
152700061009                   -5 -     (S/N)=N                        opz. 05¬
152800970526                   -6 -     (S/N)=N                        opz. 06¬
152900060315                   -7 -     (S/N)=N                        opz. 07¬
153000970527                   -8 -     (S/N)=N                        opz. 08¬
153100970527                   -9 -     (S/N)=N                        opz. 09¬
153200970527                   -10-     (S/N)=N                        opz. 10¬
153300970529                   -11-     (S/N)=N                        opz. 11¬
153400970529                   -12-     (S/N)=N                        opz. 12¬
153500060922                   -13-     (S/N)=N                        opz. 13¬
153600970526                   -  -     (S/N)=N                        opz. 14¬
153700970526                   -  -     (S/N)=N                        opz. 15¬
153800970526                   -  -     (S/N)=N                        opz. 16¬
153900970526                   -  -     (S/N)=N                        opz. 17¬
154000970526                   -  -     (S/N)=N                        opz. 18¬
154100970526                   -  -     (S/N)=N                        opz. 19¬
154200970526                   -  -     (S/N)=N                        opz. 20¬
154300970526                   -  -     (S/N)=N                        opz. 21¬
154400970526                   -  -     (S/N)=N                        opz. 22¬
154500970526                   -  -     (S/N)=N                        opz. 23¬
154600970526                   -  -     (S/N)=N                        opz. 24¬
154700970526** FUD - FUNZIONI
154800970526                   -        (S/N)=N                        funz.01¬
154900970526                   -        (S/N)=N                        funz.02¬
155000970526F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
155100970526                   -        (S/N)=N                        funz.04¬
155200970526F05=Rivisualizza   -        (S/N)=S  <== S-FISSO           funz.05¬
155300061009                   -        (S/N)=N                        funz.06¬
155400061009                   -        (S/N)=N                        funz.07¬
155500970527                   -        (S/N)=N                        funz.08¬
155600970527                   -        (S/N)=N                        funz.09¬
155700970527                   -        (S/N)=N                        funz.10¬
155800970526                   -        (S/N)=N                        funz.11¬
155900970526F12=Ritorno        -        (S/N)=S  <== S-FISSO           funz.12¬
156000060922                   -        (S/N)=N                        funz.13¬
156100970526                   -        (S/N)=N                        funz.14¬
156200970526                   -        (S/N)=N                        funz.15¬
156300970526                   -        (S/N)=N                        funz.16¬
156400970526                   -        (S/N)=N                        funz.17¬
156500970526                   -        (S/N)=N                        funz.18¬
156600970526 RU=Pagina su      -        (S/N)=S  <== S-FISSO           funz.19¬
156700970526 RD=Pagina giù     -        (S/N)=S  <== S-FISSO           funz.20¬
156800970527                   -        (S/N)=N                        funz.21¬
156900970527                   -        (S/N)=N                        funz.22¬
157000970526F23=Altre opzioni  -        (S/N)=S  <== S-FISSO           funz.23¬
157100970526F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
