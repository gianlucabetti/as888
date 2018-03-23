000100060711     FTIST63D   CF   E             WORKSTN
000200060711     FTABEL00F  IF   E           K DISK
000300041013     FTNTBE01L  IF   E           K DISK
000400060711     FWFPTL00F  IF   E           K DISK    USROPN
000500060712     FPRTEMAIL  O    F  198        PRINTER OFLIND(*INOF) USROPN
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
004200060714     D  PARDAI                        8  0
004300060714     D  PARDAF                        8  0
004400060714     D  PARFAT                       17  0
004500060714     D  PARKSC                        7
004600060714     D  PARFIL                        3  0
004700060714     D  PARNAZ                        3
004800060714     D  PARCAP                        9
004900060714     D  PARPRV                        2
005000060714     D  PARBLK                        1
005100060714     D  PARCIC                        1
005200060714     D  PARBAR                        1
005300060714     D  PARSDI                        1
005400060714     D  PARPIC                        1
005500060714     D  PARFOA                        1
005600060714     D  PARNTN1                       2
005700060714     D  PARNTE1                       2
005800060714     D  PARSOC                        3
005900060714     D  PARCOF                        1
006000060714     D  PARCPI                        1
006100060714     D  PARIND                        1
006200060714     D  PARNTN2                       2
006300060714     D  PARNTN3                       2
006400060714     D  PARNTN4                       2
006500060714     D  PARNTE2                       2
006600060714     D  PARNTE3                       2
006700060714     D  PARNTE4                       2
006800060714     D  PARNTE5                       2
006900060714     D  PARNTE6                       2
007000060714     D  PARNTE7                       2
007100060714     D  PARNTE8                       2
007200060714     D  PARTBC                        1
007300060714     D  PARRGR                        1
007400060714     D  PAROVR                        1
007500060714     D  PARCHK                        1
007600060714     D  PARRAGR                       1
007700060714     D  PARSOGN                       1
007800060711     D*------------------
007900060711     D* Variabili d wrk
008000060711     D*------------------
008100060711     D wDataISO        s               d   inz
008200060810     D OTXT1           s            132    inz
008300060810     D OTXT2           s            132    inz
008400060810     D OTXT3           s            132    inz
008500060810     D OTXT4           s            132    inz
008600060810     D OTXT5           s            132    inz
008700060810     D OTXT6           s            132    inz
008800060810     D OTXT7           s            132    inz
008900060810     D OTXT8           s            132    inz
009000060810     D OTXT9           s            132    inz
009100060810     D OTXT10          s            132    inz
009200060810     D OTXT11          s            132    inz
009300060810     D OTXT12          s            132    inz
009400060810     D OTXT13          s            132    inz
009500060810     D OTXT14          s            132    inz
009600060810     D OTXT15          s            132    inz
009700060810     D RigaStampa      s            132    inz
009800060712     D TotClienti      s              6  0 inz
009900060712     D TotMail         s              6  0 inz
010000060712     D aCapo           s              4    inz('<br>')
010100060712     D iRiga           s              6    inz('<span>')
010200060712     D fRiga           s              7    inz('</span>')
010300040224
010400040217
010500040217     C* /MAIN
010600051025     C*
010700041013     C* Reperisco dati tabellati (Tabella 'MRA')
010800041013     C                   EXSR      CARTAB
010900060711     C*
011000060711     C* Effettuo inizializzazione videata
011100060711     C                   EXSR      INZVID
011200040217     C*
011300040217     C* Ciclo finchè nn scelta opzione valida
011400040217     C                   DOW       *INKL = *OFF
011500040217     C*
011600060711     C* Emetto la window x ricevere dall'utente i parametri a video
011700060711     C                   EXFMT     ST63DW01
011800040224     C*
011900040224     C* Effettuo controllo dell'input
012000060711     C                   EXSR      CHKINPUT
012100040217     C*
012200040217     C* Se F06 elaboro
012300040217     C     *INKF         IFEQ      *ON
012400051024     C*
012500040224     C* Se tutto ok => procedo
012600040224     C                   IF        FlgOK = 'S'
012700060711     C*
012800060711     C* Duplico in QTEMP del job corrente i file x estrazione anagrafico clienti WFPTL*
012900060711     C                   CALL      'TIST63C'
013000060711     C*
013100060711     C* Estraggo l'anagrafico clienti x l'invio email corrente
013200060711     C                   EXSR      IMPPARPTL
013300060711     C                   MOVEL     PARAM         KPJBU
013400060711     C                   CALL      'TIST43R'
013500060711     C                   PARM                    KPJBA
013600060711     C*
013700060711     C* Scorro tutto il file WFPTL x generare 1 spool x ciascun cliente
013800060711     C                   OPEN      WFPTL00F
013900060711     C     *START        SETLL     WFPTL00F
014000060711     C                   READ      WFPTL00F
014100060711     C                   DOW       not %eof(WFPTL00F)
014200060712     C*
014300060712     C* Incremento il contatore anagrafiche estratte
014400060712     C                   ADD       1             TotClienti
014500060711     C*
014600060711     C* Ovviamente solo clienti con e-mail
014700060711     C                   IF        wfpEML <> *blanks
014800041006     C*
014900060711     C* Imposto quindi i dati utenti estesi
015000060711     C                   EVAL      §CM1MITT = %trim(§MRAMIT1L) +
015100060711     C                                        %trim(%editw(VIDPO:'0   '))
015200060711     C                   EVAL      §CM1VAR  = '*OBJM*' + %trim(VIDOBJ) +
015300060711     C                             ' (Cli.' + %editw(wfpKSC:'0       ') + ')'
015400041013     C                   EVAL      §CM1TIPS = §MRAREG
015500041013     C                   EVAL      §CM1IDP  = §MRAIDPRO
015600060711     C                   MOVEL(P)  VIDPO         §CM1PO
015700060711     C                   EVAL      §CM1DST  = wfpEML
015800041006     C*
015900041006     C* Eseguo override x generare uno spool ad "hoc"
016000060711     C                   CALL      'TRUL83C1'
016100060810     C                   PARM      'P'           OPZIONE           1
016200060810     C                   PARM      'PRTEMAIL'    PRTFILE          10
016300041006     C                   PARM                    TRTCM1DS
016400041013     C                   PARM                    §MRAOUTQI
016500060711     C                   PARM                    Esito             1
016600060711     C*
016700041006     C* Se nn c sono errori d override proseguo con l'elaborazione, altrimenti mando messaggio all'u
016800041006     C                   IF        Esito = '2'
016900060711     C                   EVAL      MSGERR    = 'Presenti alcuni errori in ' +
017000060711     C                                         'invio email, avvisare il CED.'
017100041006     C                   ELSE
017200060801     C*
017300060801     C* Eseguo la routine d generazione dello spool
017400060801     C                   EXSR      CRTSPOOL
017500060712     C*
017600060712     C* Incremento il contatore email generate
017700060712     C                   ADD       1             TotMail
017800060711     C                   ENDIF
017900060711     C*
018000060711     C* Eseguo delete override precedente
018100060711     C                   CALL      'TRUL83C1'
018200060810     C                   PARM      'D'           OPZIONE
018300060810     C                   PARM                    PRTFILE
018400060711     C                   PARM                    TRTCM1DS
018500060711     C                   PARM                    §MRAOUTQI
018600060711     C                   PARM                    Esito
018700060711     C*
018800060711     C                   ENDIF
018900060711     C*
019000060711     C* Proseguo con la lettura del cliente successivo
019100060711     C                   READ      WFPTL00F
019200060711     C                   ENDDO
019300060711     C                   CLOSE     WFPTL00F
019400060801     C*
019500060801     C* Oltre ai clienti invio 1 mail aggiuntiva a: 'ced' + 'tlc' + mittente mail precedenti
019600060801     C*
019700060801     C* Imposto quindi i dati utenti estesi
019800060801     C                   EVAL      §CM1MITT = %trim(§MRAMIT1L) +
019900060801     C                                        %trim(%editw(VIDPO:'0   '))
020000060801     C                   EVAL      §CM1VAR  = '*OBJM*' + %trim(VIDOBJ)
020100060801     C                   EVAL      §CM1TIPS = §MRAREG
020200060801     C                   EVAL      §CM1IDP  = §MRAIDPRO
020300060801     C                   MOVEL(P)  '046'         §CM1PO
020400120307     C                   EVAL      §CM1DST  = 'ced@brt.it; ' +
020500120307     C                                        'tlc@brt.it; ' +
020600120307     C                                        %trim(§CM1MITT)+'@brt.it'
020700060801     C*
020800060801     C* Eseguo override x generare uno spool ad "hoc"
020900060801     C                   CALL      'TRUL83C1'
021000060810     C                   PARM      'P'           OPZIONE           1
021100060810     C                   PARM      'PRTEMAIL'    PRTFILE          10
021200060801     C                   PARM                    TRTCM1DS
021300060801     C                   PARM                    §MRAOUTQI
021400060801     C                   PARM                    Esito             1
021500060801     C*
021600060801     C* Se nn c sono errori d override proseguo con l'elaborazione, altrimenti mando messaggio all'u
021700060801     C                   IF        Esito = '2'
021800060801     C                   EVAL      MSGERR    = 'Presenti alcuni errori in ' +
021900060801     C                                         'invio email, avvisare il CED.'
022000060801     C                   ELSE
022100060801     C*
022200060801     C* Eseguo la routine d generazione dello spool
022300060801     C                   EXSR      CRTSPOOL
022400060801     C*
022500060801     C* Eseguo delete override precedente
022600060801     C                   CALL      'TRUL83C1'
022700060810     C                   PARM      'D'           OPZIONE
022800060810     C                   PARM                    PRTFILE
022900060801     C                   PARM                    TRTCM1DS
023000060801     C                   PARM                    §MRAOUTQI
023100060801     C                   PARM                    Esito
023200060801     C                   ENDIF
023300060712     C*
023400060712     C* A fine elaborazione stampo il resoconto
023500060712     C                   EVAL      RigaStampa = 'TIST63R - *** Comunicazioni ' +
023600060712     C                             'via email a clienti ***'
023700060712     C                   EXCEPT    STAFIN
023800060712     C                   EVAL      RigaStampa = 'Totale clienti estratti: ' +
023900060712     C                                          %editc(TotClienti:'4')
024000060712     C                   EXCEPT    STAFIN
024100060712     C                   EVAL      RigaStampa = 'Totale email inviate:    ' +
024200060712     C                                          %editc(TotMail:'4')
024300060712     C                   EXCEPT    STAFIN
024400051104     C*
024500051104     C* Dopo l'invio della mail forzo l'uscita dal programma
024600051104     C                   EVAL      *INKL = *ON
024700060711     C*
024800060711     C                   ENDIF
024900051102     C*
025000040224     C                   ENDIF
025100040217     C*
025200040217     C* Se F12 vado a fine
025300040217     C     *INKL         IFEQ      *ON
025400040218     C                   EVAL      *INKL = *ON
025500040217     C                   ENDIF
025600040217     C*
025700040217     C                   ENDDO
025800051025     C*
025900040217     C                   SETON                                        LR
026000040217     C****************************************************************
026100060801
026200060801
026300060801
026400060801     C****************************************************************
026500060801     C* CREAZIONE DELLO SPOOL CORRENTE
026600060801     C****************************************************************
026700060801     C     CRTSPOOL      BEGSR
026800060801     C*
026900060801     C* Compongo le righe dello spool
027000060801     C                   IF        VIDTXT1 = *blanks
027100060801     C                   EVAL      VIDTXT1 = aCapo
027200060801     C                   ENDIF
027300060801     C                   IF        VIDTXT2 = *blanks
027400060801     C                   EVAL      VIDTXT2 = aCapo
027500060801     C                   ENDIF
027600060801     C                   IF        VIDTXT3 = *blanks
027700060801     C                   EVAL      VIDTXT3 = aCapo
027800060801     C                   ENDIF
027900060801     C                   IF        VIDTXT4 = *blanks
028000060801     C                   EVAL      VIDTXT4 = aCapo
028100060801     C                   ENDIF
028200060801     C                   IF        VIDTXT5 = *blanks
028300060801     C                   EVAL      VIDTXT5 = aCapo
028400060801     C                   ENDIF
028500060801     C                   IF        VIDTXT6 = *blanks
028600060801     C                   EVAL      VIDTXT6 = aCapo
028700060801     C                   ENDIF
028800060801     C                   IF        VIDTXT7 = *blanks
028900060801     C                   EVAL      VIDTXT7 = aCapo
029000060801     C                   ENDIF
029100060801     C                   IF        VIDTXT8 = *blanks
029200060801     C                   EVAL      VIDTXT8 = aCapo
029300060801     C                   ENDIF
029400060801     C                   IF        VIDTXT9 = *blanks
029500060801     C                   EVAL      VIDTXT9 = aCapo
029600060801     C                   ENDIF
029700060801     C                   IF        VIDTXT10 = *blanks
029800060801     C                   EVAL      VIDTXT10 = aCapo
029900060801     C                   ENDIF
030000060801     C                   IF        VIDTXT11 = *blanks
030100060801     C                   EVAL      VIDTXT11 = aCapo
030200060801     C                   ENDIF
030300060801     C                   IF        VIDTXT12 = *blanks
030400060801     C                   EVAL      VIDTXT12 = aCapo
030500060801     C                   ENDIF
030600060801     C                   IF        VIDTXT13 = *blanks
030700060801     C                   EVAL      VIDTXT13 = aCapo
030800060801     C                   ENDIF
030900060801     C                   IF        VIDTXT14 = *blanks
031000060801     C                   EVAL      VIDTXT14 = aCapo
031100060801     C                   ENDIF
031200060801     C                   IF        VIDTXT15 = *blanks
031300060801     C                   EVAL      VIDTXT15 = aCapo
031400060801     C                   ENDIF
031500060801     C*
031600060801     C                   EVAL      OTXT1 =iRiga+VIDTXT1 +fRiga+aCapo
031700060801     C                   EVAL      OTXT2 =iRiga+VIDTXT2 +fRiga+aCapo
031800060801     C                   EVAL      OTXT3 =iRiga+VIDTXT3 +fRiga+aCapo
031900060801     C                   EVAL      OTXT4 =iRiga+VIDTXT4 +fRiga+aCapo
032000060801     C                   EVAL      OTXT5 =iRiga+VIDTXT5 +fRiga+aCapo
032100060801     C                   EVAL      OTXT6 =iRiga+VIDTXT6 +fRiga+aCapo
032200060801     C                   EVAL      OTXT7 =iRiga+VIDTXT7 +fRiga+aCapo
032300060801     C                   EVAL      OTXT8 =iRiga+VIDTXT8 +fRiga+aCapo
032400060801     C                   EVAL      OTXT9 =iRiga+VIDTXT9 +fRiga+aCapo
032500060801     C                   EVAL      OTXT10=iRiga+VIDTXT10+fRiga+aCapo
032600060801     C                   EVAL      OTXT11=iRiga+VIDTXT11+fRiga+aCapo
032700060801     C                   EVAL      OTXT12=iRiga+VIDTXT12+fRiga+aCapo
032800060801     C                   EVAL      OTXT13=iRiga+VIDTXT13+fRiga+aCapo
032900060801     C                   EVAL      OTXT14=iRiga+VIDTXT14+fRiga+aCapo
033000060801     C                   EVAL      OTXT15=iRiga+VIDTXT15+fRiga+aCapo
033100060801     C*
033200060801     C                   EVAL      OTXT1='§§Riga01§§'+'#'+%trim(OTXT1)+' }{'
033300060801     C                   EVAL      OTXT2='§§Riga02§§'+'#'+%trim(OTXT2)+' }{'
033400060801     C                   EVAL      OTXT3='§§Riga03§§'+'#'+%trim(OTXT3)+' }{'
033500060801     C                   EVAL      OTXT4='§§Riga04§§'+'#'+%trim(OTXT4)+' }{'
033600060801     C                   EVAL      OTXT5='§§Riga05§§'+'#'+%trim(OTXT5)+' }{'
033700060801     C                   EVAL      OTXT6='§§Riga06§§'+'#'+%trim(OTXT6)+' }{'
033800060801     C                   EVAL      OTXT7='§§Riga07§§'+'#'+%trim(OTXT7)+' }{'
033900060801     C                   EVAL      OTXT8='§§Riga08§§'+'#'+%trim(OTXT8)+' }{'
034000060801     C                   EVAL      OTXT9='§§Riga09§§'+'#'+%trim(OTXT9)+' }{'
034100060801     C                   EVAL      OTXT10='§§Riga10§§'+'#'+%trim(OTXT10)+' }{'
034200060801     C                   EVAL      OTXT11='§§Riga11§§'+'#'+%trim(OTXT11)+' }{'
034300060801     C                   EVAL      OTXT12='§§Riga12§§'+'#'+%trim(OTXT12)+' }{'
034400060801     C                   EVAL      OTXT13='§§Riga13§§'+'#'+%trim(OTXT13)+' }{'
034500060801     C                   EVAL      OTXT14='§§Riga14§§'+'#'+%trim(OTXT14)+' }{'
034600060801     C                   EVAL      OTXT15='§§Riga15§§'+'#'+%trim(OTXT15)+' }{'
034700060801     C*
034800060801     C* Creo lo spool
034900060801     C                   OPEN      PRTEMAIL
035000060801     C                   EXCEPT    DET
035100060801     C                   CLOSE     PRTEMAIL
035200060801     C*
035300060801     C                   ENDSR
035400060801     C***************************************************************
035500040524
035600040524
035700040524
035800040524     C****************************************************************
035900040524     C* INIZIALIZZA LA VIDEATA CON I VALORI D DEFAULT
036000040524     C****************************************************************
036100040524     C     INZVID        BEGSR
036200060711     C*
036300060711     C                   CLEAR                   ST63DW01
036400040524     C*
036500040524     C                   ENDSR
036600040524     C***************************************************************
036700040217
036800040217
036900040217     C****************************************************************
037000040217     C     CHKINPUT      BEGSR
037100040217     C****************************************************************
037200040224     C*
037300040224     C                   CLEAR                   MSGERR
037400040217     C*
037500040217     C* Inizializzo il flag d controllo input
037600040217     C                   MOVEL     'S'           FlgOK             1
037700041013     C*
037800041013     C* Spengo gli indicatori d errore a video
037900060711     C                   SETOFF                                       313233
038000041013     C*
038100041013     C* Effettuo controllo su reperimento con successo record 'TRUL83R' in tabella 'MRA'
038200041013     C                   IF        *IN60 = *on
038300060711     C                   SETON                                        313233
038400041013     C                   MOVEL     'N'           FlgOK
038500060711     C                   EVAL      MSGERR = 'Record TIST63R non trovato in ' +
038600041013     C                                      'tabella MRA, avvisare il CED.'
038700041013     C                   ENDIF
038800040217     C*
038900040217     C* Effettuo controlli obbligatorietà parametri a video
039000060904     C                   IF        FlgOK = 'S'
039100060904     C                   IF        VIDPO  = *zeros
039200060904     C                   SETON                                        31
039300060904     C                   MOVEL     'N'           FlgOK
039400060904     C                   EVAL      MSGERR = 'Campo obbligatorio'
039500060904     C                   ENDIF
039600060904     C                   ENDIF
039700040224     C                   IF        FlgOK = 'S'
039800060711     C                   IF        VIDOBJ = *blanks
039900041005     C                   SETON                                        32
040000041005     C                   MOVEL     'N'           FlgOK
040100041005     C                   EVAL      MSGERR = 'Campo obbligatorio'
040200041005     C                   ENDIF
040300041005     C                   ENDIF
040400041005     C                   IF        FlgOK = 'S'
040500060711     C                   IF        VIDTXT1+VIDTXT2+VIDTXT3+VIDTXT4+VIDTXT5 +
040600060711     C                             VIDTXT6+VIDTXT7+VIDTXT8+VIDTXT9+VIDTXT10+
040700060711     C                             VIDTXT11+VIDTXT12+VIDTXT13+VIDTXT14+VIDTXT15=
040800060711     C                             *blanks
040900041005     C                   SETON                                        33
041000041005     C                   MOVEL     'N'           FlgOK
041100041005     C                   EVAL      MSGERR = 'Campo obbligatorio'
041200041005     C                   ENDIF
041300041005     C                   ENDIF
041400040217     C*
041500040217     C                   ENDSR
041600040217     C****************************************************************
041700060711
041800060711
041900060711
042000060711     C****************************************************************
042100060711     C* IMPOSTA I PARAMETRI X L'ESTRAZIONE ANAGRAFICO CLIENTI
042200060711     C****************************************************************
042300060711     C     IMPPARPTL     BEGSR
042400060711     C*
042500060711     C                   CLEAR                   PARAM
042600060711     C*
042700060711     C* REPERISCO L'ULTIMA DATA SALDI CLIENTE
042800060711     C                   Z-ADD     1             tblKUT
042900060711     C                   MOVEL     '5S'          tblCOD
043000060711     C                   MOVEL     *BLANKS       tblKEY
043100060711     C*
043200060711     C     KEYTABEL      CHAIN     TABEL00F                           30
043300060711     C                   IF        *IN30 = *ON OR TBLFLG <> ' '
043400060711     C                   Z-ADD     DATCOR        PARDAF
043500060711     C                   ELSE
043600060711     C                   EVAL      DS5S = TBLUNI
043700060711     C                   Z-ADD     §5SDAC        PARDAF
043800060711     C                   ENDIF
043900060711     C*
044000060711     C* Sottraggo 1 mese dalla data finale (ultimi saldi estratti)
044100060711     C     *ISO          MOVE      PARDAF        wDataIso
044200060714     C     wDataIso      SUBDUR    2:*M          wDataIso
044300060711     C     *ISO          MOVE      wDataIso      PARDAI
044400060711     C*
044500060711     C                   MOVEL     *blanks       PARKSC
044600060711     C                   MOVEL     *blanks       PARBLK
044700060711     C                   MOVEL     'S'           PARTBC
044800060711     C                   MOVEL     'N'           PARRGR
044900060711     C                   MOVEL     'N'           PAROVR
045000060711     C                   MOVEL     'S'           PARCHK
045100060711     C                   MOVEL     'S'           PARSOGN
045200060711     C                   Z-ADD     VIDPO         PARFIL
045300060711     C                   MOVEL     *blanks       PARSOC
045400060711     C                   MOVEL     'C'           PARCOF
045500060711     C                   MOVEL     '999'         PARNAZ
045600060711     C                   MOVEL     *blanks       PARCAP
045700060711     C                   MOVEL     *blanks       PARPRV
045800060714     C                   Z-ADD     250           PARFAT
045900060711     C                   MOVEL     'N'           PARCPI
046000060711     C                   MOVEL     'S'           PARBAR
046100060711     C                   MOVEL     'S'           PARSDI
046200060711     C                   MOVEL     'N'           PARPIC
046300060714     C                   MOVEL     'S'           PARFOA
046400060711     C                   MOVEL     '06'          PARNTE1
046500060711     C                   MOVEL     '03'          PARNTE2
046600060711     C                   MOVEL     '88'          PARNTE3
046700060711     C                   MOVEL     '85'          PARNTE4
046800060711     C                   MOVEL     '84'          PARNTE5
046900060711     C                   MOVEL     '87'          PARNTE6
047000060711     C                   MOVEL     '08'          PARNTE7
047100060711     C                   MOVEL     *blanks       PARNTE8
047200060711     C                   MOVEL     'S'           PARIND
047300060711     C                   MOVEL     'M'           PARRAGR
047400060711     C*
047500060711     C                   ENDSR
047600060711     C***************************************************************
047700040217
047800040217
047900040217     C****************************************************************
048000040217     C* REPERISCE I DATI UTENTE
048100040217     C****************************************************************
048200040217     C     REPDATIUTE    BEGSR
048300040217     C*
048400040217     C* INIZIALIZZA VARIABILI DI WRK
048500040217     C                   CLEAR                   TIBS34DS
048600040217     C                   CLEAR                   AZUTEDS
048700040217     C                   CLEAR                   DDATIUTE
048800040217     C*
048900040217     C     *DTAARA       DEFINE    §azute        azuteds
049000040217     C     *DTAARA       DEFINE    §datiute      ddatiute
049100040217     C                   IN(E)     *DTAARA
049200040217     C                   IF        %Error
049300040217     C                   EVAL      I34Tla = 'L'
049400040217     C                   CALL      'TIBS34R'
049500040217     C                   PARM                    Tibs34Ds
049600040217     C                   IN        *DTAARA
049700040217     C                   ENDIF
049800040217     C*
049900040217     C                   ENDSR
050000040217     C***************************************************************
050100041013
050200041013
050300041013
050400041013     C****************************************************************
050500041013     C* CARICA DATI TABELLATI
050600041013     C****************************************************************
050700041013     C     CARTAB        BEGSR
050800041013     C*
050900041013     C* Reperimento tabella MRA
051000041013     C                   CLEAR                   DMRA
051100041013     C                   MOVE(P)   'MRA'         tbeCOD
051200060711     C                   MOVEL(p)  'TIST63R'     tbeKE1
051300041013     C                   MOVE      *blanks       tbeKE2
051400041013     C     KEYTBE01P     CHAIN     TNTBE01L
051500041013     C                   IF        %found(TNTBE01L) AND
051600041013     C                             tbeATB <> 'A'
051700041013     C                   SETOFF                                       60
051800041013     C                   MOVEL     tbeUNI        DMRA
051900041013     C                   ELSE
052000041013     C                   SETON                                        60
052100051025     C                   EVAL      Esito = '2'
052200060711     C                   EVAL      MSGERR    = 'Record TIST63R non trovato in '+
052300051025     C                                      'tabella MRA, avvisare il CED.'
052400041013     C                   ENDIF
052500041013     C*
052600041013     C                   ENDSR
052700041013     C***************************************************************
052800040217
052900040217
053000040217     C***************************************************************
053100040130     C     *INZSR        BEGSR
053200040217     C***************************************************************
053300040217     C*
053400041006     C     *ENTRY        PLIST
053500060711     C                   PARM                    KPJBA
053600040524     C*
053700040524     C* Reperisco il P.O. dell'utente corrente
053800040524     C                   EXSR      REPDATIUTE
053900060711     C*
054000060711     C* CALCOLA LA DATA CORRENTE
054100060711     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
054200060711     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
054300060711     C                   Z-ADD     WN8           G08DAT
054400060711     C                   Z-ADD     *ZEROS        G08INV
054500060711     C                   MOVEL     '0'           G08ERR
054600060711     C                   CALL      'XSRDA8'
054700060711     C                   PARM                    WLBDA8
054800060711     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
054900040218     C*
055000060711     C* CHIAVI DI LETTURA
055100060711     C* TNTBE01L
055200041013     C     KEYTBE01P     KLIST
055300041013     C                   KFLD                    tbeCOD
055400041013     C                   KFLD                    tbeKE1
055500041013     C                   KFLD                    tbeKE2
055600060711     C* TABEL00F
055700060711     C     KEYTABEL      KLIST
055800060711     C                   KFLD                    tblKUT
055900060711     C                   KFLD                    tblCOD
056000060711     C                   KFLD                    tblKEY
056100040217     C*
056200040130     C                   ENDSR
056300041006     O*------------------------------------------------------------------------*
056400041006     O* STAMPA
056500041006     O*------------------------------------------------------------------------*
056600041006     OPRTEMAIL  E            DET         1
056700060711     O                       OTXT1               +1
056800041006     O          E            DET         1
056900060711     O                       OTXT2               +1
057000041006     O          E            DET         1
057100060711     O                       OTXT3               +1
057200041006     O          E            DET         1
057300060711     O                       OTXT4               +1
057400041006     O          E            DET         1
057500060711     O                       OTXT5               +1
057600041006     O          E            DET         1
057700060711     O                       OTXT6               +1
057800041006     O          E            DET         1
057900060711     O                       OTXT7               +1
058000060711     O          E            DET         1
058100060711     O                       OTXT8               +1
058200060711     O          E            DET         1
058300060711     O                       OTXT9               +1
058400060711     O          E            DET         1
058500060711     O                       OTXT10              +1
058600060711     O          E            DET         1
058700060711     O                       OTXT11              +1
058800060711     O          E            DET         1
058900060711     O                       OTXT12              +1
059000060711     O          E            DET         1
059100060711     O                       OTXT13              +1
059200060711     O          E            DET         1
059300060711     O                       OTXT14              +1
059400060711     O          E            DET         1
059500060711     O                       OTXT15              +1
059600060712     O*------------------------------------------------------------------------*
059700060712     OPRTF198   E            STAFIN      1
059800060712     O                       RigaStampa
