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
008200060711     D OTXT1           s            100    inz
008300060711     D OTXT2           s            100    inz
008400060711     D OTXT3           s            100    inz
008500060711     D OTXT4           s            100    inz
008600060711     D OTXT5           s            100    inz
008700060711     D OTXT6           s            100    inz
008800060711     D OTXT7           s            100    inz
008900060711     D OTXT8           s            100    inz
009000060711     D OTXT9           s            100    inz
009100060711     D OTXT10          s            100    inz
009200060711     D OTXT11          s            100    inz
009300060711     D OTXT12          s            100    inz
009400060711     D OTXT13          s            100    inz
009500060711     D OTXT14          s            100    inz
009600060711     D OTXT15          s            100    inz
009700060712     D RigaStampa      s            100    inz
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
017000060712     C                   EVAL      OTXT1='§§Riga01§§'+'#'+%trim(VIDTXT1)+
017100060712     C                                   'vbCRLF' + '}{'
017200060712     C                   EVAL      OTXT2='§§Riga02§§'+'#'+%trim(VIDTXT2)+
017300060712     C                                   'vbCRLF' + '}{'
017400060712     C                   EVAL      OTXT3='§§Riga03§§'+'#'+%trim(VIDTXT3)+
017500060712     C                                   'vbCRLF' + '}{'
017600060712     C                   EVAL      OTXT4='§§Riga04§§'+'#'+%trim(VIDTXT4)+
017700060712     C                                   'vbCRLF' + '}{'
017800060712     C                   EVAL      OTXT5='§§Riga05§§'+'#'+%trim(VIDTXT5)+
017900060712     C                                   'vbCRLF' + '}{'
018000060712     C                   EVAL      OTXT6='§§Riga06§§'+'#'+%trim(VIDTXT6)+
018100060712     C                                   'vbCRLF' + '}{'
018200060712     C                   EVAL      OTXT7='§§Riga07§§'+'#'+%trim(VIDTXT7)+
018300060712     C                                   'vbCRLF' + '}{'
018400060712     C                   EVAL      OTXT8='§§Riga08§§'+'#'+%trim(VIDTXT8)+
018500060712     C                                   'vbCRLF' + '}{'
018600060712     C                   EVAL      OTXT9='§§Riga09§§'+'#'+%trim(VIDTXT9)+
018700060712     C                                   'vbCRLF' + '}{'
018800060712     C                   EVAL      OTXT10='§§Riga10§§'+'#'+%trim(VIDTXT10)+
018900060712     C                                   'vbCRLF' + '}{'
019000060712     C                   EVAL      OTXT11='§§Riga11§§'+'#'+%trim(VIDTXT11)+
019100060712     C                                   'vbCRLF' + '}{'
019200060712     C                   EVAL      OTXT12='§§Riga12§§'+'#'+%trim(VIDTXT12)+
019300060712     C                                   'vbCRLF' + '}{'
019400060712     C                   EVAL      OTXT13='§§Riga13§§'+'#'+%trim(VIDTXT13)+
019500060712     C                                   'vbCRLF' + '}{'
019600060712     C                   EVAL      OTXT14='§§Riga14§§'+'#'+%trim(VIDTXT14)+
019700060712     C                                   'vbCRLF' + '}{'
019800060712     C                   EVAL      OTXT15='§§Riga15§§'+'#'+%trim(VIDTXT15)+
019900060712     C                                   'vbCRLF' + '}{'
020000060711     C*
020100060711     C* Creo lo spool
020200041006     C                   OPEN      PRTEMAIL
020300041006     C                   EXCEPT    DET
020400041006     C                   CLOSE     PRTEMAIL
020500060712     C*
020600060712     C* Incremento il contatore email generate
020700060712     C                   ADD       1             TotMail
020800060711     C                   ENDIF
020900060711     C*
021000060711     C* Eseguo delete override precedente
021100060711     C                   CALL      'TRUL83C1'
021200060711     C                   PARM      'D'           OPZIONE           1
021300060711     C                   PARM                    TRTCM1DS
021400060711     C                   PARM                    §MRAOUTQI
021500060711     C                   PARM                    Esito
021600060711     C*
021700060711     C                   ENDIF
021800060711     C*
021900060711     C* Proseguo con la lettura del cliente successivo
022000060711     C                   READ      WFPTL00F
022100060711     C                   ENDDO
022200060711     C                   CLOSE     WFPTL00F
022300060712     C*
022400060712     C* A fine elaborazione stampo il resoconto
022500060712     C                   EVAL      RigaStampa = 'TIST63R - *** Comunicazioni ' +
022600060712     C                             'via email a clienti ***'
022700060712     C                   EXCEPT    STAFIN
022800060712     C                   EVAL      RigaStampa = 'Totale clienti estratti: ' +
022900060712     C                                          %editc(TotClienti:'4')
023000060712     C                   EXCEPT    STAFIN
023100060712     C                   EVAL      RigaStampa = 'Totale email inviate:    ' +
023200060712     C                                          %editc(TotMail:'4')
023300060712     C                   EXCEPT    STAFIN
023400051104     C*
023500051104     C* Dopo l'invio della mail forzo l'uscita dal programma
023600051104     C                   EVAL      *INKL = *ON
023700060711     C*
023800060711     C                   ENDIF
023900051102     C*
024000040224     C                   ENDIF
024100040217     C*
024200040217     C* Se F12 vado a fine
024300040217     C     *INKL         IFEQ      *ON
024400040218     C                   EVAL      *INKL = *ON
024500040217     C                   ENDIF
024600040217     C*
024700040217     C                   ENDDO
024800051025     C*
024900040217     C                   SETON                                        LR
025000040217     C****************************************************************
025100040524
025200040524
025300040524
025400040524     C****************************************************************
025500040524     C* INIZIALIZZA LA VIDEATA CON I VALORI D DEFAULT
025600040524     C****************************************************************
025700040524     C     INZVID        BEGSR
025800060711     C*
025900060711     C                   CLEAR                   ST63DW01
026000040524     C*
026100040524     C                   ENDSR
026200040524     C***************************************************************
026300040217
026400040217
026500040217     C****************************************************************
026600040217     C     CHKINPUT      BEGSR
026700040217     C****************************************************************
026800040224     C*
026900040224     C                   CLEAR                   MSGERR
027000040217     C*
027100040217     C* Inizializzo il flag d controllo input
027200040217     C                   MOVEL     'S'           FlgOK             1
027300041013     C*
027400041013     C* Spengo gli indicatori d errore a video
027500060711     C                   SETOFF                                       313233
027600041013     C*
027700041013     C* Effettuo controllo su reperimento con successo record 'TRUL83R' in tabella 'MRA'
027800041013     C                   IF        *IN60 = *on
027900060711     C                   SETON                                        313233
028000041013     C                   MOVEL     'N'           FlgOK
028100060711     C                   EVAL      MSGERR = 'Record TIST63R non trovato in ' +
028200041013     C                                      'tabella MRA, avvisare il CED.'
028300041013     C                   ENDIF
028400040217     C*
028500040217     C* Effettuo controlli obbligatorietà parametri a video
028600040224     C                   IF        FlgOK = 'S'
028700060711     C                   IF        VIDOBJ = *blanks
028800041005     C                   SETON                                        32
028900041005     C                   MOVEL     'N'           FlgOK
029000041005     C                   EVAL      MSGERR = 'Campo obbligatorio'
029100041005     C                   ENDIF
029200041005     C                   ENDIF
029300041005     C                   IF        FlgOK = 'S'
029400060711     C                   IF        VIDTXT1+VIDTXT2+VIDTXT3+VIDTXT4+VIDTXT5 +
029500060711     C                             VIDTXT6+VIDTXT7+VIDTXT8+VIDTXT9+VIDTXT10+
029600060711     C                             VIDTXT11+VIDTXT12+VIDTXT13+VIDTXT14+VIDTXT15=
029700060711     C                             *blanks
029800041005     C                   SETON                                        33
029900041005     C                   MOVEL     'N'           FlgOK
030000041005     C                   EVAL      MSGERR = 'Campo obbligatorio'
030100041005     C                   ENDIF
030200041005     C                   ENDIF
030300040217     C*
030400040217     C                   ENDSR
030500040217     C****************************************************************
030600060711
030700060711
030800060711
030900060711     C****************************************************************
031000060711     C* IMPOSTA I PARAMETRI X L'ESTRAZIONE ANAGRAFICO CLIENTI
031100060711     C****************************************************************
031200060711     C     IMPPARPTL     BEGSR
031300060711     C*
031400060711     C                   CLEAR                   PARAM
031500060711     C*
031600060711     C* REPERISCO L'ULTIMA DATA SALDI CLIENTE
031700060711     C                   Z-ADD     1             tblKUT
031800060711     C                   MOVEL     '5S'          tblCOD
031900060711     C                   MOVEL     *BLANKS       tblKEY
032000060711     C*
032100060711     C     KEYTABEL      CHAIN     TABEL00F                           30
032200060711     C                   IF        *IN30 = *ON OR TBLFLG <> ' '
032300060711     C                   Z-ADD     DATCOR        PARDAF
032400060711     C                   ELSE
032500060711     C                   EVAL      DS5S = TBLUNI
032600060711     C                   Z-ADD     §5SDAC        PARDAF
032700060711     C                   ENDIF
032800060711     C*
032900060711     C* Sottraggo 1 mese dalla data finale (ultimi saldi estratti)
033000060711     C     *ISO          MOVE      PARDAF        wDataIso
033100060711     C     wDataIso      SUBDUR    1:*M          wDataIso
033200060711     C     *ISO          MOVE      wDataIso      PARDAI
033300060711     C*
033400060711     C                   MOVEL     *blanks       PARKSC
033500060711     C                   MOVEL     *blanks       PARBLK
033600060711     C                   MOVEL     'S'           PARTBC
033700060711     C                   MOVEL     'N'           PARRGR
033800060711     C                   MOVEL     'N'           PAROVR
033900060711     C                   MOVEL     'S'           PARCHK
034000060711     C                   MOVEL     'S'           PARSOGN
034100060711     C                   Z-ADD     VIDPO         PARFIL
034200060711     C                   MOVEL     *blanks       PARSOC
034300060711     C                   MOVEL     'C'           PARCOF
034400060711     C                   MOVEL     '999'         PARNAZ
034500060711     C                   MOVEL     *blanks       PARCAP
034600060711     C                   MOVEL     *blanks       PARPRV
034700060711     C                   Z-ADD     250           PARFAT
034800060711     C                   MOVEL     'N'           PARCPI
034900060711     C                   MOVEL     'S'           PARBAR
035000060711     C                   MOVEL     'S'           PARSDI
035100060711     C                   MOVEL     'N'           PARPIC
035200060711     C                   MOVEL     '06'          PARNTE1
035300060711     C                   MOVEL     '03'          PARNTE2
035400060711     C                   MOVEL     '88'          PARNTE3
035500060711     C                   MOVEL     '85'          PARNTE4
035600060711     C                   MOVEL     '84'          PARNTE5
035700060711     C                   MOVEL     '87'          PARNTE6
035800060711     C                   MOVEL     '08'          PARNTE7
035900060711     C                   MOVEL     *blanks       PARNTE8
036000060711     C                   MOVEL     'S'           PARIND
036100060711     C                   MOVEL     'M'           PARRAGR
036200060711     C*
036300060711     C                   ENDSR
036400060711     C***************************************************************
036500040217
036600040217
036700040217     C****************************************************************
036800040217     C* REPERISCE I DATI UTENTE
036900040217     C****************************************************************
037000040217     C     REPDATIUTE    BEGSR
037100040217     C*
037200040217     C* INIZIALIZZA VARIABILI DI WRK
037300040217     C                   CLEAR                   TIBS34DS
037400040217     C                   CLEAR                   AZUTEDS
037500040217     C                   CLEAR                   DDATIUTE
037600040217     C*
037700040217     C     *DTAARA       DEFINE    §azute        azuteds
037800040217     C     *DTAARA       DEFINE    §datiute      ddatiute
037900040217     C                   IN(E)     *DTAARA
038000040217     C                   IF        %Error
038100040217     C                   EVAL      I34Tla = 'L'
038200040217     C                   CALL      'TIBS34R'
038300040217     C                   PARM                    Tibs34Ds
038400040217     C                   IN        *DTAARA
038500040217     C                   ENDIF
038600040217     C*
038700040217     C                   ENDSR
038800040217     C***************************************************************
038900041013
039000041013
039100041013
039200041013     C****************************************************************
039300041013     C* CARICA DATI TABELLATI
039400041013     C****************************************************************
039500041013     C     CARTAB        BEGSR
039600041013     C*
039700041013     C* Reperimento tabella MRA
039800041013     C                   CLEAR                   DMRA
039900041013     C                   MOVE(P)   'MRA'         tbeCOD
040000060711     C                   MOVEL(p)  'TIST63R'     tbeKE1
040100041013     C                   MOVE      *blanks       tbeKE2
040200041013     C     KEYTBE01P     CHAIN     TNTBE01L
040300041013     C                   IF        %found(TNTBE01L) AND
040400041013     C                             tbeATB <> 'A'
040500041013     C                   SETOFF                                       60
040600041013     C                   MOVEL     tbeUNI        DMRA
040700041013     C                   ELSE
040800041013     C                   SETON                                        60
040900051025     C                   EVAL      Esito = '2'
041000060711     C                   EVAL      MSGERR    = 'Record TIST63R non trovato in '+
041100051025     C                                      'tabella MRA, avvisare il CED.'
041200041013     C                   ENDIF
041300041013     C*
041400041013     C                   ENDSR
041500041013     C***************************************************************
041600040217
041700040217
041800040217     C***************************************************************
041900040130     C     *INZSR        BEGSR
042000040217     C***************************************************************
042100040217     C*
042200041006     C     *ENTRY        PLIST
042300060711     C                   PARM                    KPJBA
042400040524     C*
042500040524     C* Reperisco il P.O. dell'utente corrente
042600040524     C                   EXSR      REPDATIUTE
042700060711     C*
042800060711     C* CALCOLA LA DATA CORRENTE
042900060711     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
043000060711     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
043100060711     C                   Z-ADD     WN8           G08DAT
043200060711     C                   Z-ADD     *ZEROS        G08INV
043300060711     C                   MOVEL     '0'           G08ERR
043400060711     C                   CALL      'XSRDA8'
043500060711     C                   PARM                    WLBDA8
043600060711     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
043700040218     C*
043800060711     C* CHIAVI DI LETTURA
043900060711     C* TNTBE01L
044000041013     C     KEYTBE01P     KLIST
044100041013     C                   KFLD                    tbeCOD
044200041013     C                   KFLD                    tbeKE1
044300041013     C                   KFLD                    tbeKE2
044400060711     C* TABEL00F
044500060711     C     KEYTABEL      KLIST
044600060711     C                   KFLD                    tblKUT
044700060711     C                   KFLD                    tblCOD
044800060711     C                   KFLD                    tblKEY
044900040217     C*
045000040130     C                   ENDSR
045100041006     O*------------------------------------------------------------------------*
045200041006     O* STAMPA
045300041006     O*------------------------------------------------------------------------*
045400041006     OPRTEMAIL  E            DET         1
045500060711     O                       OTXT1               +1
045600041006     O          E            DET         1
045700060711     O                       OTXT2               +1
045800041006     O          E            DET         1
045900060711     O                       OTXT3               +1
046000041006     O          E            DET         1
046100060711     O                       OTXT4               +1
046200041006     O          E            DET         1
046300060711     O                       OTXT5               +1
046400041006     O          E            DET         1
046500060711     O                       OTXT6               +1
046600041006     O          E            DET         1
046700060711     O                       OTXT7               +1
046800060711     O          E            DET         1
046900060711     O                       OTXT8               +1
047000060711     O          E            DET         1
047100060711     O                       OTXT9               +1
047200060711     O          E            DET         1
047300060711     O                       OTXT10              +1
047400060711     O          E            DET         1
047500060711     O                       OTXT11              +1
047600060711     O          E            DET         1
047700060711     O                       OTXT12              +1
047800060711     O          E            DET         1
047900060711     O                       OTXT13              +1
048000060711     O          E            DET         1
048100060711     O                       OTXT14              +1
048200060711     O          E            DET         1
048300060711     O                       OTXT15              +1
048400060712     O*------------------------------------------------------------------------*
048500060712     OPRTF198   E            STAFIN         1 02
048600060712     O                       RigaStampa          +1
