000100060711     FTIST63D   CF   E             WORKSTN
000200060711     FTABEL00F  IF   E           K DISK
000300041013     FTNTBE01L  IF   E           K DISK
000400060711     FWFPTL00F  IF   E           K DISK    USROPN
000500060711     FPRTEMAIL  O    F  132        PRINTER OFLIND(*INOF) USROPN
000600060712     FPRTF198   O    F  198        PRINTER
000700040928      **********************************************************
000800041006     D*------------------
000900041006     D* ARCHITETTURA
001000041006     D*------------------
001100041006     D KPJBA         E DS
001200041013     D*------------------
001300041013     D* TABELLEA 'MRA'
001400041013     D*------------------
001500060711     D DMRA          E DS
001600060711     D*------------------
001700040217     D* DS ridefinizione dati utente estesi spool x mailing
001800060711     D*------------------
001900040217     D TRTCM1DS      E DS
002000060711     D*-------------------
002100060711     D* DS DATI STATISTICHE/SALDI SEDE
002200060711     D*-------------------
002300060711     D DS5S          E DS
002400060711     D*------------------
002500040217     D* DS REPERIMENTO DATI UTENTE
002600060711     D*------------------
002700040217     D TIBS34DS      E DS                                                       *Profili utente
002800040217     D DDATIUTE      E DS                                                       *Dati utente
002900040217     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
003000060711     D*------------------
003100060711     D* DS "XSRDA8" - CONTROLLA DATA (8)
003200060711     D*------------------
003300060711     D WLBDA8          DS                  INZ
003400060711     D  G08DAT                 1      8  0
003500060711     D  G08INV                 9     16  0
003600060711     D  G08ERR                17     17
003700060711     D  G08TGI                18     22  0
003800060711     D*-------------------
003900060711     D* DS PARAMETRI DI LANCIO PGM ESTRAZIONE ANAGRAFICO CLIENTI
004000060711     D*-------------------
004100060711     D PARAM           DS
004200060711     D  PARDAI                 1      8  0
004300060711     D  PARDAF                 9     16  0
004400060711     D  PARFAT                17     33  0
004500060711     D  PARKSC                34     40
004600060711     D  PARFIL                41     43  0
004700060711     D  PARNAZ                44     46
004800060711     D  PARCAP                47     55
004900060711     D  PARPRV                56     57
005000060711     D  PARBLK                58     58
005100060711     D  PARCIC                59     59
005200060711     D  PARBAR                60     60
005300060711     D  PARSDI                61     61
005400060711     D  PARPIC                62     62
005500060711     D  PARFOA                63     63
005600060711     D  PARNTN1               64     65
005700060711     D  PARNTE1               66     67
005800060711     D  PARSOC                68     70
005900060711     D  PARCOF                71     71
006000060711     D  PARCPI                72     72
006100060711     D  PARIND                73     73
006200060711     D  PARNTN2               74     75
006300060711     D  PARNTN3               76     77
006400060711     D  PARNTN4               78     79
006500060711     D  PARNTE2               80     81
006600060711     D  PARNTE3               82     83
006700060711     D  PARNTE4               84     85
006800060711     D  PARNTE5               86     87
006900060711     D  PARNTE6               88     99
007000060711     D  PARNTE7               90     91
007100060711     D  PARNTE8               92     93
007200060711     D  PARTBC                94     94
007300060711     D  PARRGR                95     95
007400060711     D  PAROVR                96     96
007500060711     D  PARCHK                97     97
007600060711     D  PARRAGR               98     98
007700060711     D  PARSOGN               99     99
007800060711     D*------------------
007900060711     D* Variabili d wrk
008000060711     D*------------------
008100060711     D wDataISO        s               d   inz
008200060712     D OTXT1           s            120    inz
008300060712     D OTXT2           s            120    inz
008400060712     D OTXT3           s            120    inz
008500060712     D OTXT4           s            120    inz
008600060712     D OTXT5           s            120    inz
008700060712     D OTXT6           s            120    inz
008800060712     D OTXT7           s            120    inz
008900060712     D OTXT8           s            120    inz
009000060712     D OTXT9           s            120    inz
009100060712     D OTXT10          s            120    inz
009200060712     D OTXT11          s            120    inz
009300060712     D OTXT12          s            120    inz
009400060712     D OTXT13          s            120    inz
009500060712     D OTXT14          s            120    inz
009600060712     D OTXT15          s            120    inz
009700060712     D RigaStampa      s            120    inz
009800060712     D TotClienti      s              6  0 inz
009900060712     D TotMail         s              6  0 inz
010000040224
010100040217
010200040217     C* /MAIN
010300051025     C*
010400041013     C* Reperisco dati tabellati (Tabella 'MRA')
010500041013     C                   EXSR      CARTAB
010600060711     C*
010700060711     C* Effettuo inizializzazione videata
010800060711     C                   EXSR      INZVID
010900040217     C*
011000040217     C* Ciclo finchè nn scelta opzione valida
011100040217     C                   DOW       *INKL = *OFF
011200040217     C*
011300060711     C* Emetto la window x ricevere dall'utente i parametri a video
011400060711     C                   EXFMT     ST63DW01
011500040224     C*
011600040224     C* Effettuo controllo dell'input
011700060711     C                   EXSR      CHKINPUT
011800040217     C*
011900040217     C* Se F06 elaboro
012000040217     C     *INKF         IFEQ      *ON
012100051024     C*
012200040224     C* Se tutto ok => procedo
012300040224     C                   IF        FlgOK = 'S'
012400060711     C*
012500060711     C* Duplico in QTEMP del job corrente i file x estrazione anagrafico clienti WFPTL*
012600060711     C                   CALL      'TIST63C'
012700060711     C*
012800060711     C* Estraggo l'anagrafico clienti x l'invio email corrente
012900060711     C                   EXSR      IMPPARPTL
013000060711     C                   MOVEL     PARAM         KPJBU
013100060711     C                   CALL      'TIST43R'
013200060711     C                   PARM                    KPJBA
013300060711     C*
013400060711     C* Scorro tutto il file WFPTL x generare 1 spool x ciascun cliente
013500060711     C                   OPEN      WFPTL00F
013600060711     C     *START        SETLL     WFPTL00F
013700060711     C                   READ      WFPTL00F
013800060711     C                   DOW       not %eof(WFPTL00F)
013900060712     C*
014000060712     C* Incremento il contatore anagrafiche estratte
014100060712     C                   ADD       1             TotClienti
014200060711     C*
014300060711     C* Ovviamente solo clienti con e-mail
014400060711     C                   IF        wfpEML <> *blanks
014500041006     C*
014600060711     C* Imposto quindi i dati utenti estesi
014700060711     C                   EVAL      §CM1MITT = %trim(§MRAMIT1L) +
014800060711     C                                        %trim(%editw(VIDPO:'0   '))
014900060711     C                   EVAL      §CM1VAR  = '*OBJM*' + %trim(VIDOBJ) +
015000060711     C                             ' (Cli.' + %editw(wfpKSC:'0       ') + ')'
015100041013     C                   EVAL      §CM1TIPS = §MRAREG
015200041013     C                   EVAL      §CM1IDP  = §MRAIDPRO
015300060711     C                   MOVEL(P)  VIDPO         §CM1PO
015400060711     C                   EVAL      §CM1DST  = wfpEML
015500041006     C*
015600041006     C* Eseguo override x generare uno spool ad "hoc"
015700060711     C                   CALL      'TRUL83C1'
015800060711     C                   PARM      'O'           OPZIONE           1
015900041006     C                   PARM                    TRTCM1DS
016000041013     C                   PARM                    §MRAOUTQI
016100060711     C                   PARM                    Esito             1
016200060711     C*
016300041006     C* Se nn c sono errori d override proseguo con l'elaborazione, altrimenti mando messaggio all'u
016400041006     C                   IF        Esito = '2'
016500060711     C                   EVAL      MSGERR    = 'Presenti alcuni errori in ' +
016600060711     C                                         'invio email, avvisare il CED.'
016700041006     C                   ELSE
016800060711     C*
016900060711     C* Compongo le righe dello spool
017000060712     C                   IF        VIDTXT1 = *blanks
017100060712     C                   EVAL      VIDTXT1 = '<br>'
017200060712     C                   ENDIF
017300060712     C                   IF        VIDTXT2 = *blanks
017400060712     C                   EVAL      VIDTXT2 = '<br>'
017500060712     C                   ENDIF
017600060712     C                   IF        VIDTXT3 = *blanks
017700060712     C                   EVAL      VIDTXT3 = '<br>'
017800060712     C                   ENDIF
017900060712     C                   IF        VIDTXT4 = *blanks
018000060712     C                   EVAL      VIDTXT4 = '<br>'
018100060712     C                   ENDIF
018200060712     C                   IF        VIDTXT5 = *blanks
018300060712     C                   EVAL      VIDTXT5 = '<br>'
018400060712     C                   ENDIF
018500060712     C                   IF        VIDTXT6 = *blanks
018600060712     C                   EVAL      VIDTXT6 = '<br>'
018700060712     C                   ENDIF
018800060712     C                   IF        VIDTXT7 = *blanks
018900060712     C                   EVAL      VIDTXT7 = '<br>'
019000060712     C                   ENDIF
019100060712     C                   IF        VIDTXT8 = *blanks
019200060712     C                   EVAL      VIDTXT8 = '<br>'
019300060712     C                   ENDIF
019400060712     C                   IF        VIDTXT9 = *blanks
019500060712     C                   EVAL      VIDTXT9 = '<br>'
019600060712     C                   ENDIF
019700060712     C                   IF        VIDTXT10 = *blanks
019800060712     C                   EVAL      VIDTXT10 = '<br>'
019900060712     C                   ENDIF
020000060712     C                   IF        VIDTXT11 = *blanks
020100060712     C                   EVAL      VIDTXT11 = '<br>'
020200060712     C                   ENDIF
020300060712     C                   IF        VIDTXT12 = *blanks
020400060712     C                   EVAL      VIDTXT12 = '<br>'
020500060712     C                   ENDIF
020600060712     C                   IF        VIDTXT13 = *blanks
020700060712     C                   EVAL      VIDTXT13 = '<br>'
020800060712     C                   ENDIF
020900060712     C                   IF        VIDTXT14 = *blanks
021000060712     C                   EVAL      VIDTXT14 = '<br>'
021100060712     C                   ENDIF
021200060712     C                   IF        VIDTXT15 = *blanks
021300060712     C                   EVAL      VIDTXT15 = '<br>'
021400060712     C                   ENDIF
021500060712     C*
021600060712     C                   EVAL      OTXT1 ='<div>' + %trim(VIDTXT1)  + '</div>'
021700060712     C                   EVAL      OTXT2 ='<div>' + %trim(VIDTXT2)  + '</div>'
021800060712     C                   EVAL      OTXT3 ='<div>' + %trim(VIDTXT3)  + '</div>'
021900060712     C                   EVAL      OTXT4 ='<div>' + %trim(VIDTXT4)  + '</div>'
022000060712     C                   EVAL      OTXT5 ='<div>' + %trim(VIDTXT5)  + '</div>'
022100060712     C                   EVAL      OTXT6 ='<div>' + %trim(VIDTXT6)  + '</div>'
022200060712     C                   EVAL      OTXT7 ='<div>' + %trim(VIDTXT7)  + '</div>'
022300060712     C                   EVAL      OTXT8 ='<div>' + %trim(VIDTXT8)  + '</div>'
022400060712     C                   EVAL      OTXT9 ='<div>' + %trim(VIDTXT9)  + '</div>'
022500060712     C                   EVAL      OTXT10='<div>' + %trim(VIDTXT10) + '</div>'
022600060712     C                   EVAL      OTXT11='<div>' + %trim(VIDTXT11) + '</div>'
022700060712     C                   EVAL      OTXT12='<div>' + %trim(VIDTXT12) + '</div>'
022800060712     C                   EVAL      OTXT13='<div>' + %trim(VIDTXT13) + '</div>'
022900060712     C                   EVAL      OTXT14='<div>' + %trim(VIDTXT14) + '</div>'
023000060712     C                   EVAL      OTXT15='<div>' + %trim(VIDTXT15) + '</div>'
023100060712     C*
023200060712     C                   EVAL      OTXT1='§§Riga01§§'+'#'+%trim(OTXT1)+' }{'
023300060712     C                   EVAL      OTXT2='§§Riga02§§'+'#'+%trim(OTXT2)+' }{'
023400060712     C                   EVAL      OTXT3='§§Riga03§§'+'#'+%trim(OTXT3)+' }{'
023500060712     C                   EVAL      OTXT4='§§Riga04§§'+'#'+%trim(OTXT4)+' }{'
023600060712     C                   EVAL      OTXT5='§§Riga05§§'+'#'+%trim(OTXT5)+' }{'
023700060712     C                   EVAL      OTXT6='§§Riga06§§'+'#'+%trim(OTXT6)+' }{'
023800060712     C                   EVAL      OTXT7='§§Riga07§§'+'#'+%trim(OTXT7)+' }{'
023900060712     C                   EVAL      OTXT8='§§Riga08§§'+'#'+%trim(OTXT8)+' }{'
024000060712     C                   EVAL      OTXT9='§§Riga09§§'+'#'+%trim(OTXT9)+' }{'
024100060712     C                   EVAL      OTXT10='§§Riga10§§'+'#'+%trim(OTXT10)+' }{'
024200060712     C                   EVAL      OTXT11='§§Riga11§§'+'#'+%trim(OTXT11)+' }{'
024300060712     C                   EVAL      OTXT12='§§Riga12§§'+'#'+%trim(OTXT12)+' }{'
024400060712     C                   EVAL      OTXT13='§§Riga13§§'+'#'+%trim(OTXT13)+' }{'
024500060712     C                   EVAL      OTXT14='§§Riga14§§'+'#'+%trim(OTXT14)+' }{'
024600060712     C                   EVAL      OTXT15='§§Riga15§§'+'#'+%trim(OTXT15)+' }{'
024700060711     C*
024800060711     C* Creo lo spool
024900041006     C                   OPEN      PRTEMAIL
025000041006     C                   EXCEPT    DET
025100041006     C                   CLOSE     PRTEMAIL
025200060712     C*
025300060712     C* Incremento il contatore email generate
025400060712     C                   ADD       1             TotMail
025500060711     C                   ENDIF
025600060711     C*
025700060711     C* Eseguo delete override precedente
025800060711     C                   CALL      'TRUL83C1'
025900060711     C                   PARM      'D'           OPZIONE           1
026000060711     C                   PARM                    TRTCM1DS
026100060711     C                   PARM                    §MRAOUTQI
026200060711     C                   PARM                    Esito
026300060711     C*
026400060711     C                   ENDIF
026500060711     C*
026600060711     C* Proseguo con la lettura del cliente successivo
026700060711     C                   READ      WFPTL00F
026800060711     C                   ENDDO
026900060711     C                   CLOSE     WFPTL00F
027000060712     C*
027100060712     C* A fine elaborazione stampo il resoconto
027200060712     C                   EVAL      RigaStampa = 'TIST63R - *** Comunicazioni ' +
027300060712     C                             'via email a clienti ***'
027400060712     C                   EXCEPT    STAFIN
027500060712     C                   EVAL      RigaStampa = 'Totale clienti estratti: ' +
027600060712     C                                          %editc(TotClienti:'4')
027700060712     C                   EXCEPT    STAFIN
027800060712     C                   EVAL      RigaStampa = 'Totale email inviate:    ' +
027900060712     C                                          %editc(TotMail:'4')
028000060712     C                   EXCEPT    STAFIN
028100051104     C*
028200051104     C* Dopo l'invio della mail forzo l'uscita dal programma
028300051104     C                   EVAL      *INKL = *ON
028400060711     C*
028500060711     C                   ENDIF
028600051102     C*
028700040224     C                   ENDIF
028800040217     C*
028900040217     C* Se F12 vado a fine
029000040217     C     *INKL         IFEQ      *ON
029100040218     C                   EVAL      *INKL = *ON
029200040217     C                   ENDIF
029300040217     C*
029400040217     C                   ENDDO
029500051025     C*
029600040217     C                   SETON                                        LR
029700040217     C****************************************************************
029800040524
029900040524
030000040524
030100040524     C****************************************************************
030200040524     C* INIZIALIZZA LA VIDEATA CON I VALORI D DEFAULT
030300040524     C****************************************************************
030400040524     C     INZVID        BEGSR
030500060711     C*
030600060711     C                   CLEAR                   ST63DW01
030700040524     C*
030800040524     C                   ENDSR
030900040524     C***************************************************************
031000040217
031100040217
031200040217     C****************************************************************
031300040217     C     CHKINPUT      BEGSR
031400040217     C****************************************************************
031500040224     C*
031600040224     C                   CLEAR                   MSGERR
031700040217     C*
031800040217     C* Inizializzo il flag d controllo input
031900040217     C                   MOVEL     'S'           FlgOK             1
032000041013     C*
032100041013     C* Spengo gli indicatori d errore a video
032200060711     C                   SETOFF                                       313233
032300041013     C*
032400041013     C* Effettuo controllo su reperimento con successo record 'TRUL83R' in tabella 'MRA'
032500041013     C                   IF        *IN60 = *on
032600060711     C                   SETON                                        313233
032700041013     C                   MOVEL     'N'           FlgOK
032800060711     C                   EVAL      MSGERR = 'Record TIST63R non trovato in ' +
032900041013     C                                      'tabella MRA, avvisare il CED.'
033000041013     C                   ENDIF
033100040217     C*
033200040217     C* Effettuo controlli obbligatorietà parametri a video
033300040224     C                   IF        FlgOK = 'S'
033400060711     C                   IF        VIDOBJ = *blanks
033500041005     C                   SETON                                        32
033600041005     C                   MOVEL     'N'           FlgOK
033700041005     C                   EVAL      MSGERR = 'Campo obbligatorio'
033800041005     C                   ENDIF
033900041005     C                   ENDIF
034000041005     C                   IF        FlgOK = 'S'
034100060711     C                   IF        VIDTXT1+VIDTXT2+VIDTXT3+VIDTXT4+VIDTXT5 +
034200060711     C                             VIDTXT6+VIDTXT7+VIDTXT8+VIDTXT9+VIDTXT10+
034300060711     C                             VIDTXT11+VIDTXT12+VIDTXT13+VIDTXT14+VIDTXT15=
034400060711     C                             *blanks
034500041005     C                   SETON                                        33
034600041005     C                   MOVEL     'N'           FlgOK
034700041005     C                   EVAL      MSGERR = 'Campo obbligatorio'
034800041005     C                   ENDIF
034900041005     C                   ENDIF
035000040217     C*
035100040217     C                   ENDSR
035200040217     C****************************************************************
035300060711
035400060711
035500060711
035600060711     C****************************************************************
035700060711     C* IMPOSTA I PARAMETRI X L'ESTRAZIONE ANAGRAFICO CLIENTI
035800060711     C****************************************************************
035900060711     C     IMPPARPTL     BEGSR
036000060711     C*
036100060711     C                   CLEAR                   PARAM
036200060711     C*
036300060711     C* REPERISCO L'ULTIMA DATA SALDI CLIENTE
036400060711     C                   Z-ADD     1             tblKUT
036500060711     C                   MOVEL     '5S'          tblCOD
036600060711     C                   MOVEL     *BLANKS       tblKEY
036700060711     C*
036800060711     C     KEYTABEL      CHAIN     TABEL00F                           30
036900060711     C                   IF        *IN30 = *ON OR TBLFLG <> ' '
037000060711     C                   Z-ADD     DATCOR        PARDAF
037100060711     C                   ELSE
037200060711     C                   EVAL      DS5S = TBLUNI
037300060711     C                   Z-ADD     §5SDAC        PARDAF
037400060711     C                   ENDIF
037500060711     C*
037600060711     C* Sottraggo 1 mese dalla data finale (ultimi saldi estratti)
037700060711     C     *ISO          MOVE      PARDAF        wDataIso
037800060711     C     wDataIso      SUBDUR    1:*M          wDataIso
037900060711     C     *ISO          MOVE      wDataIso      PARDAI
038000060711     C*
038100060711     C                   MOVEL     *blanks       PARKSC
038200060711     C                   MOVEL     *blanks       PARBLK
038300060711     C                   MOVEL     'S'           PARTBC
038400060711     C                   MOVEL     'N'           PARRGR
038500060711     C                   MOVEL     'N'           PAROVR
038600060711     C                   MOVEL     'S'           PARCHK
038700060711     C                   MOVEL     'S'           PARSOGN
038800060711     C                   Z-ADD     VIDPO         PARFIL
038900060711     C                   MOVEL     *blanks       PARSOC
039000060711     C                   MOVEL     'C'           PARCOF
039100060711     C                   MOVEL     '999'         PARNAZ
039200060711     C                   MOVEL     *blanks       PARCAP
039300060711     C                   MOVEL     *blanks       PARPRV
039400060711     C                   Z-ADD     250           PARFAT
039500060711     C                   MOVEL     'N'           PARCPI
039600060711     C                   MOVEL     'S'           PARBAR
039700060711     C                   MOVEL     'S'           PARSDI
039800060711     C                   MOVEL     'N'           PARPIC
039900060711     C                   MOVEL     '06'          PARNTE1
040000060711     C                   MOVEL     '03'          PARNTE2
040100060711     C                   MOVEL     '88'          PARNTE3
040200060711     C                   MOVEL     '85'          PARNTE4
040300060711     C                   MOVEL     '84'          PARNTE5
040400060711     C                   MOVEL     '87'          PARNTE6
040500060711     C                   MOVEL     '08'          PARNTE7
040600060711     C                   MOVEL     *blanks       PARNTE8
040700060711     C                   MOVEL     'S'           PARIND
040800060711     C                   MOVEL     'M'           PARRAGR
040900060711     C*
041000060711     C                   ENDSR
041100060711     C***************************************************************
041200040217
041300040217
041400040217     C****************************************************************
041500040217     C* REPERISCE I DATI UTENTE
041600040217     C****************************************************************
041700040217     C     REPDATIUTE    BEGSR
041800040217     C*
041900040217     C* INIZIALIZZA VARIABILI DI WRK
042000040217     C                   CLEAR                   TIBS34DS
042100040217     C                   CLEAR                   AZUTEDS
042200040217     C                   CLEAR                   DDATIUTE
042300040217     C*
042400040217     C     *DTAARA       DEFINE    §azute        azuteds
042500040217     C     *DTAARA       DEFINE    §datiute      ddatiute
042600040217     C                   IN(E)     *DTAARA
042700040217     C                   IF        %Error
042800040217     C                   EVAL      I34Tla = 'L'
042900040217     C                   CALL      'TIBS34R'
043000040217     C                   PARM                    Tibs34Ds
043100040217     C                   IN        *DTAARA
043200040217     C                   ENDIF
043300040217     C*
043400040217     C                   ENDSR
043500040217     C***************************************************************
043600041013
043700041013
043800041013
043900041013     C****************************************************************
044000041013     C* CARICA DATI TABELLATI
044100041013     C****************************************************************
044200041013     C     CARTAB        BEGSR
044300041013     C*
044400041013     C* Reperimento tabella MRA
044500041013     C                   CLEAR                   DMRA
044600041013     C                   MOVE(P)   'MRA'         tbeCOD
044700060711     C                   MOVEL(p)  'TIST63R'     tbeKE1
044800041013     C                   MOVE      *blanks       tbeKE2
044900041013     C     KEYTBE01P     CHAIN     TNTBE01L
045000041013     C                   IF        %found(TNTBE01L) AND
045100041013     C                             tbeATB <> 'A'
045200041013     C                   SETOFF                                       60
045300041013     C                   MOVEL     tbeUNI        DMRA
045400041013     C                   ELSE
045500041013     C                   SETON                                        60
045600051025     C                   EVAL      Esito = '2'
045700060711     C                   EVAL      MSGERR    = 'Record TIST63R non trovato in '+
045800051025     C                                      'tabella MRA, avvisare il CED.'
045900041013     C                   ENDIF
046000041013     C*
046100041013     C                   ENDSR
046200041013     C***************************************************************
046300040217
046400040217
046500040217     C***************************************************************
046600040130     C     *INZSR        BEGSR
046700040217     C***************************************************************
046800040217     C*
046900041006     C     *ENTRY        PLIST
047000060711     C                   PARM                    KPJBA
047100040524     C*
047200040524     C* Reperisco il P.O. dell'utente corrente
047300040524     C                   EXSR      REPDATIUTE
047400060711     C*
047500060711     C* CALCOLA LA DATA CORRENTE
047600060711     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
047700060711     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
047800060711     C                   Z-ADD     WN8           G08DAT
047900060711     C                   Z-ADD     *ZEROS        G08INV
048000060711     C                   MOVEL     '0'           G08ERR
048100060711     C                   CALL      'XSRDA8'
048200060711     C                   PARM                    WLBDA8
048300060711     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
048400040218     C*
048500060711     C* CHIAVI DI LETTURA
048600060711     C* TNTBE01L
048700041013     C     KEYTBE01P     KLIST
048800041013     C                   KFLD                    tbeCOD
048900041013     C                   KFLD                    tbeKE1
049000041013     C                   KFLD                    tbeKE2
049100060711     C* TABEL00F
049200060711     C     KEYTABEL      KLIST
049300060711     C                   KFLD                    tblKUT
049400060711     C                   KFLD                    tblCOD
049500060711     C                   KFLD                    tblKEY
049600040217     C*
049700040130     C                   ENDSR
049800041006     O*------------------------------------------------------------------------*
049900041006     O* STAMPA
050000041006     O*------------------------------------------------------------------------*
050100041006     OPRTEMAIL  E            DET         1
050200060711     O                       OTXT1               +1
050300041006     O          E            DET         1
050400060711     O                       OTXT2               +1
050500041006     O          E            DET         1
050600060711     O                       OTXT3               +1
050700041006     O          E            DET         1
050800060711     O                       OTXT4               +1
050900041006     O          E            DET         1
051000060711     O                       OTXT5               +1
051100041006     O          E            DET         1
051200060711     O                       OTXT6               +1
051300041006     O          E            DET         1
051400060711     O                       OTXT7               +1
051500060711     O          E            DET         1
051600060711     O                       OTXT8               +1
051700060711     O          E            DET         1
051800060711     O                       OTXT9               +1
051900060711     O          E            DET         1
052000060711     O                       OTXT10              +1
052100060711     O          E            DET         1
052200060711     O                       OTXT11              +1
052300060711     O          E            DET         1
052400060711     O                       OTXT12              +1
052500060711     O          E            DET         1
052600060711     O                       OTXT13              +1
052700060711     O          E            DET         1
052800060711     O                       OTXT14              +1
052900060711     O          E            DET         1
053000060711     O                       OTXT15              +1
053100060712     O*------------------------------------------------------------------------*
053200060712     OPRTF198   E            STAFIN         1 02
053300060712     O                       RigaStampa          +1
