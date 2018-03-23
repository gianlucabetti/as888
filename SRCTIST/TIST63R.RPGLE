000100120628
000200120628     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000300120628     h dftactgrp(*no) BNDDIR('TRBM') ACTGRP('BARTVAS')
000400120628
000500060711     FTIST63D   CF   E             WORKSTN
000600060711     FTABEL00F  IF   E           K DISK
000700041013     FTNTBE01L  IF   E           K DISK
000800060711     FWFPTL00F  IF   E           K DISK    USROPN
000900060712     FPRTF198   O    F  198        PRINTER
001000040928      **********************************************************
001100041006     D*------------------
001200041006     D* ARCHITETTURA
001300041006     D*------------------
001400041006     D KPJBA         E DS
001500041013     D*------------------
001600041013     D* TABELLEA 'MRA'
001700041013     D*------------------
001800120628     D DMRA          E DS                  prefix(X_)
001900060711     D*------------------
002000040217     D* DS ridefinizione dati utente estesi spool x mailing
002100060711     D*------------------
002200120628     D TRTCM1DS      E DS                  prefix(obs_)
002300060711     D*-------------------
002400060711     D* DS DATI STATISTICHE/SALDI SEDE
002500060711     D*-------------------
002600060711     D DS5S          E DS
002700060711     D*------------------
002800040217     D* DS REPERIMENTO DATI UTENTE
002900060711     D*------------------
003000040217     D TIBS34DS      E DS                                                       *Profili utente
003100040217     D DDATIUTE      E DS                                                       *Dati utente
003200040217     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
003300060711     D*------------------
003400060711     D* DS "XSRDA8" - CONTROLLA DATA (8)
003500060711     D*------------------
003600060711     D WLBDA8          DS                  INZ
003700060711     D  G08DAT                 1      8  0
003800060711     D  G08INV                 9     16  0
003900060711     D  G08ERR                17     17
004000060711     D  G08TGI                18     22  0
004100060711     D*-------------------
004200060711     D* DS PARAMETRI DI LANCIO PGM ESTRAZIONE ANAGRAFICO CLIENTI
004300060711     D*-------------------
004400060711     D PARAM           DS
004500060714     D  PARDAI                        8  0
004600060714     D  PARDAF                        8  0
004700060714     D  PARFAT                       17  0
004800060714     D  PARKSC                        7
004900060714     D  PARFIL                        3  0
005000060714     D  PARNAZ                        3
005100060714     D  PARCAP                        9
005200060714     D  PARPRV                        2
005300060714     D  PARBLK                        1
005400060714     D  PARCIC                        1
005500060714     D  PARBAR                        1
005600060714     D  PARSDI                        1
005700060714     D  PARPIC                        1
005800060714     D  PARFOA                        1
005900060714     D  PARNTN1                       2
006000060714     D  PARNTE1                       2
006100060714     D  PARSOC                        3
006200060714     D  PARCOF                        1
006300060714     D  PARCPI                        1
006400060714     D  PARIND                        1
006500060714     D  PARNTN2                       2
006600060714     D  PARNTN3                       2
006700060714     D  PARNTN4                       2
006800060714     D  PARNTE2                       2
006900060714     D  PARNTE3                       2
007000060714     D  PARNTE4                       2
007100060714     D  PARNTE5                       2
007200060714     D  PARNTE6                       2
007300060714     D  PARNTE7                       2
007400060714     D  PARNTE8                       2
007500060714     D  PARTBC                        1
007600060714     D  PARRGR                        1
007700060714     D  PAROVR                        1
007800060714     D  PARCHK                        1
007900060714     D  PARRAGR                       1
008000060714     D  PARSOGN                       1
008100060711     D*------------------
008200060711     D* Variabili d wrk
008300060711     D*------------------
008400060711     D wDataISO        s               d   inz
008500060810     D OTXT1           s            132    inz
008600060810     D OTXT2           s            132    inz
008700060810     D OTXT3           s            132    inz
008800060810     D OTXT4           s            132    inz
008900060810     D OTXT5           s            132    inz
009000060810     D OTXT6           s            132    inz
009100060810     D OTXT7           s            132    inz
009200060810     D OTXT8           s            132    inz
009300060810     D OTXT9           s            132    inz
009400060810     D OTXT10          s            132    inz
009500060810     D OTXT11          s            132    inz
009600060810     D OTXT12          s            132    inz
009700060810     D OTXT13          s            132    inz
009800060810     D OTXT14          s            132    inz
009900060810     D OTXT15          s            132    inz
010000060810     D RigaStampa      s            132    inz
010100060712     D TotClienti      s              6  0 inz
010200060712     D TotMail         s              6  0 inz
010300060712     D aCapo           s              4    inz('<br>')
010400060712     D iRiga           s              6    inz('<span>')
010500060712     D fRiga           s              7    inz('</span>')
010600120628     D inTags          s          65535A   VARYING
010700120628     d string          s            200a   varying
010800120628     d Esito           s             10I 0
010900040224
011000120628      // Definizione prototipi procedure.
011100120628
011200120628     D/COPY GAITRASRC/SRCPROTOPR,TRBMBYPSR
011300120628     D trul33r         PR
011400120628     D                                     EXTPGM('TRUL33R')
011500120628     D  kpjba
011600120628     D                                     LIKEDS(kpjba)
011700120628     d trul33ds      e ds
011800040217
011900040217     C* /MAIN
012000051025     C*
012100041013     C* Reperisco dati tabellati (Tabella 'MRA')
012200041013     C                   EXSR      CARTAB
012300060711     C*
012400060711     C* Effettuo inizializzazione videata
012500060711     C                   EXSR      INZVID
012600040217     C*
012700040217     C* Ciclo finchè nn scelta opzione valida
012800040217     C                   DOW       *INKL = *OFF
012900040217     C*
013000060711     C* Emetto la window x ricevere dall'utente i parametri a video
013100060711     C                   EXFMT     ST63DW01
013200040224     C*
013300040224     C* Effettuo controllo dell'input
013400060711     C                   EXSR      CHKINPUT
013500040217     C*
013600040217     C* Se F06 elaboro
013700040217     C     *INKF         IFEQ      *ON
013800051024     C*
013900040224     C* Se tutto ok => procedo
014000040224     C                   IF        FlgOK = 'S'
014100060711     C*
014200060711     C* Duplico in QTEMP del job corrente i file x estrazione anagrafico clienti WFPTL*
014300060711     C                   CALL      'TIST63C'
014400060711     C*
014500060711     C* Estraggo l'anagrafico clienti x l'invio email corrente
014600060711     C                   EXSR      IMPPARPTL
014700060711     C                   MOVEL     PARAM         KPJBU
014800060711     C                   CALL      'TIST43R'
014900060711     C                   PARM                    KPJBA
015000060711     C*
015100060711     C* Scorro tutto il file WFPTL x generare 1 spool x ciascun cliente
015200060711     C                   OPEN      WFPTL00F
015300060711     C     *START        SETLL     WFPTL00F
015400060711     C                   READ      WFPTL00F
015500060711     C                   DOW       not %eof(WFPTL00F)
015600060712     C*
015700060712     C* Incremento il contatore anagrafiche estratte
015800060712     C                   ADD       1             TotClienti
015900060711     C*
016000060711     C* Ovviamente solo clienti con e-mail
016100060711     C                   IF        wfpEML <> *blanks
016200041006     C*
016300060711     C* Imposto quindi i dati utenti estesi
016400120628     C                   EVAL      §CM1MITT = %trim(x_§MRAMIT1L) +
016500060711     C                                        %trim(%editw(VIDPO:'0   '))
016600060711     C                   EVAL      §CM1VAR  = '*OBJM*' + %trim(VIDOBJ) +
016700060711     C                             ' (Cli.' + %editw(wfpKSC:'0       ') + ')'
016800120628     C                   EVAL      §CM1TIPS = x_§MRAREG
016900120628     C                   EVAL      §CM1IDP  = x_§MRAIDPRO
017000060711     C                   MOVEL(P)  VIDPO         §CM1PO
017100060711     C                   EVAL      §CM1DST  = wfpEML
017200120628     C                   clear                   trul33ds
017300120628     C                   EVAL      i33Cnu = 24
017400120628     C                   EVAL      i33num = 1
017500120628     C                   EVAL      kpjbu = trul33ds
017600120628     C                   CALLP     trul33r(kpjba)
017700120628     C                   EVAL      trul33ds = kpjbu
017800120628     C                   EVAL      §CM1PROG=%subst(%editc(o33nri:'X'):9:7)
017900120628     C                   EVAL      §CM1ATT='01'
018000120628     C                   EVAL      §CM1TOTATT='01'
018100120628     C*
018200120628     C* creo le tags per le e-mail senza spool
018300120628     C                   EXSR      CrtTags
018400120628     C*
018500120628     C                   EVAL      Esito = 0
018600120628     C*
018700120628     C                   CALLP     Bartmail_insert
018800120628     C                             (inTags:indmrabyps:intrtcm1ds:
018900120628     C                              %len(inTags):Esito)
019000060711     C*
019100120628     C* Se ci sono errori mando messaggio all'utente
019200120628     C                   IF        Esito <> 0
019300060711     C                   EVAL      MSGERR    = 'Presenti alcuni errori in ' +
019400060711     C                                         'invio email, avvisare il CED.'
019500041006     C                   ELSE
019600060712     C*
019700060712     C* Incremento il contatore email generate
019800060712     C                   ADD       1             TotMail
019900060711     C                   ENDIF
020000060711     C*
020100060711     C                   ENDIF
020200060711     C*
020300060711     C* Proseguo con la lettura del cliente successivo
020400060711     C                   READ      WFPTL00F
020500060711     C                   ENDDO
020600060711     C                   CLOSE     WFPTL00F
020700060801     C*
020800060801     C* Oltre ai clienti invio 1 mail aggiuntiva a: 'ced' + 'tlc' + mittente mail precedenti
020900060801     C*
021000060801     C* Imposto quindi i dati utenti estesi
021100120628     C                   EVAL      §CM1MITT = %trim(x_§MRAMIT1L) +
021200060801     C                                        %trim(%editw(VIDPO:'0   '))
021300060801     C                   EVAL      §CM1VAR  = '*OBJM*' + %trim(VIDOBJ)
021400120628     C                   EVAL      §CM1TIPS = x_§MRAREG
021500120628     C                   EVAL      §CM1IDP  = x_§MRAIDPRO
021600060801     C                   MOVEL(P)  '046'         §CM1PO
021700120307     C                   EVAL      §CM1DST  = 'ced@brt.it; ' +
021800120307     C                                        'tlc@brt.it; ' +
021900120307     C                                        %trim(§CM1MITT)+'@brt.it'
022000120628     C                   clear                   trul33ds
022100120628     C                   EVAL      i33Cnu = 24
022200120628     C                   EVAL      i33num = 1
022300120628     C                   EVAL      kpjbu = trul33ds
022400120628     C                   CALLP     trul33r(kpjba)
022500120628     C                   EVAL      trul33ds = kpjbu
022600120628     C                   EVAL      §CM1PROG=%subst(%editc(o33nri:'X'):9:7)
022700120628     C                   EVAL      §CM1ATT='01'
022800120628     C                   EVAL      §CM1TOTATT='01'
022900120628     C*
023000120628     C                   EVAL      Esito = 0
023100120628     C*
023200120628     C                   CALLP     Bartmail_insert
023300120628     C                             (inTags:indmrabyps:intrtcm1ds:
023400120628     C                              %len(inTags):Esito)
023500120628     C*
023600120628     C* Se ci sono errori mando messaggio all'utente
023700120628     C                   IF        Esito <> 0
023800120628     C                   EVAL      MSGERR    = 'Presenti alcuni errori in ' +
023900120628     C                                         'invio email, avvisare il CED.'
024000120628     C                   ENDIF
024100060712     C*
024200060712     C* A fine elaborazione stampo il resoconto
024300060712     C                   EVAL      RigaStampa = 'TIST63R - *** Comunicazioni ' +
024400060712     C                             'via email a clienti ***'
024500060712     C                   EXCEPT    STAFIN
024600060712     C                   EVAL      RigaStampa = 'Totale clienti estratti: ' +
024700060712     C                                          %editc(TotClienti:'4')
024800060712     C                   EXCEPT    STAFIN
024900060712     C                   EVAL      RigaStampa = 'Totale email inviate:    ' +
025000060712     C                                          %editc(TotMail:'4')
025100060712     C                   EXCEPT    STAFIN
025200051104     C*
025300051104     C* Dopo l'invio della mail forzo l'uscita dal programma
025400051104     C                   EVAL      *INKL = *ON
025500060711     C*
025600060711     C                   ENDIF
025700051102     C*
025800040224     C                   ENDIF
025900040217     C*
026000040217     C* Se F12 vado a fine
026100040217     C     *INKL         IFEQ      *ON
026200040218     C                   EVAL      *INKL = *ON
026300040217     C                   ENDIF
026400040217     C*
026500040217     C                   ENDDO
026600051025     C*
026700040217     C                   SETON                                        LR
026800040217     C****************************************************************
026900060801
027000120628     C*-------------------------------------------------------------*
027100120628     C* controllo stringa                                           *
027200120628     C*-------------------------------------------------------------*
027300120628     C     sr_chkstr     Begsr
027400120628     C                   EVAL      string=%xlate('||':'  ':string)
027500120628     C                   EVAL      string=%xlate('==':'  ':string)
027600120628     C                   endsr
027700120628
027800120628     C****************************************************************
027900120628     C* CREAZIONE Tags
028000120628     C****************************************************************
028100120628     C     CrtTags       BEGSR
028200120628     C*
028300120628     C* Compongo le Tags
028400120628     C                   IF        VIDTXT1 = *blanks
028500120628     C                   EVAL      VIDTXT1 = aCapo
028600120628     C                   ENDIF
028700120628     C                   IF        VIDTXT2 = *blanks
028800120628     C                   EVAL      VIDTXT2 = aCapo
028900120628     C                   ENDIF
029000120628     C                   IF        VIDTXT3 = *blanks
029100120628     C                   EVAL      VIDTXT3 = aCapo
029200120628     C                   ENDIF
029300120628     C                   IF        VIDTXT4 = *blanks
029400120628     C                   EVAL      VIDTXT4 = aCapo
029500120628     C                   ENDIF
029600120628     C                   IF        VIDTXT5 = *blanks
029700120628     C                   EVAL      VIDTXT5 = aCapo
029800120628     C                   ENDIF
029900120628     C                   IF        VIDTXT6 = *blanks
030000120628     C                   EVAL      VIDTXT6 = aCapo
030100120628     C                   ENDIF
030200120628     C                   IF        VIDTXT7 = *blanks
030300120628     C                   EVAL      VIDTXT7 = aCapo
030400120628     C                   ENDIF
030500120628     C                   IF        VIDTXT8 = *blanks
030600120628     C                   EVAL      VIDTXT8 = aCapo
030700120628     C                   ENDIF
030800120628     C                   IF        VIDTXT9 = *blanks
030900120628     C                   EVAL      VIDTXT9 = aCapo
031000120628     C                   ENDIF
031100120628     C                   IF        VIDTXT10 = *blanks
031200120628     C                   EVAL      VIDTXT10 = aCapo
031300120628     C                   ENDIF
031400120628     C                   IF        VIDTXT11 = *blanks
031500120628     C                   EVAL      VIDTXT11 = aCapo
031600120628     C                   ENDIF
031700120628     C                   IF        VIDTXT12 = *blanks
031800120628     C                   EVAL      VIDTXT12 = aCapo
031900120628     C                   ENDIF
032000120628     C                   IF        VIDTXT13 = *blanks
032100120628     C                   EVAL      VIDTXT13 = aCapo
032200120628     C                   ENDIF
032300120628     C                   IF        VIDTXT14 = *blanks
032400120628     C                   EVAL      VIDTXT14 = aCapo
032500120628     C                   ENDIF
032600120628     C                   IF        VIDTXT15 = *blanks
032700120628     C                   EVAL      VIDTXT15 = aCapo
032800120628     C                   ENDIF
032900120628     C*
033000120628     C                   EVAL      OTXT1 =iRiga+%trim(VIDTXT1)+fRiga+aCapo
033100120628     C                   EVAL      OTXT2 =iRiga+%trim(VIDTXT2)+fRiga+aCapo
033200120628     C                   EVAL      OTXT3 =iRiga+%trim(VIDTXT3)+fRiga+aCapo
033300120628     C                   EVAL      OTXT4 =iRiga+%trim(VIDTXT4)+fRiga+aCapo
033400120628     C                   EVAL      OTXT5 =iRiga+%trim(VIDTXT5)+fRiga+aCapo
033500120628     C                   EVAL      OTXT6 =iRiga+%trim(VIDTXT6)+fRiga+aCapo
033600120628     C                   EVAL      OTXT7 =iRiga+%trim(VIDTXT7)+fRiga+aCapo
033700120628     C                   EVAL      OTXT8 =iRiga+%trim(VIDTXT8)+fRiga+aCapo
033800120628     C                   EVAL      OTXT9 =iRiga+%trim(VIDTXT9)+fRiga+aCapo
033900120628     C                   EVAL      OTXT10=iRiga+%trim(VIDTXT10)+fRiga+aCapo
034000120628     C                   EVAL      OTXT11=iRiga+%trim(VIDTXT11)+fRiga+aCapo
034100120628     C                   EVAL      OTXT12=iRiga+%trim(VIDTXT12)+fRiga+aCapo
034200120628     C                   EVAL      OTXT13=iRiga+%trim(VIDTXT13)+fRiga+aCapo
034300120628     C                   EVAL      OTXT14=iRiga+%trim(VIDTXT14)+fRiga+aCapo
034400120628     C                   EVAL      OTXT15=iRiga+%trim(VIDTXT15)+fRiga+aCapo
034500120628     C*
034600120628     C                   EVAL      inTags='§§Riga01§§'+'=='+%trim(OTXT1)+' ||'+
034700120628     C                                   '§§Riga02§§'+'=='+%trim(OTXT2)+' ||'+
034800120628     C                                   '§§Riga03§§'+'=='+%trim(OTXT3)+' ||'+
034900120628     C                                   '§§Riga04§§'+'=='+%trim(OTXT4)+' ||'+
035000120628     C                                   '§§Riga05§§'+'=='+%trim(OTXT5)+' ||'+
035100120628     C                                   '§§Riga06§§'+'=='+%trim(OTXT6)+' ||'+
035200120628     C                                   '§§Riga07§§'+'=='+%trim(OTXT7)+' ||'+
035300120628     C                                   '§§Riga08§§'+'=='+%trim(OTXT8)+' ||'+
035400120628     C                                   '§§Riga09§§'+'=='+%trim(OTXT9)+' ||'+
035500120628     C                                   '§§Riga10§§'+'=='+%trim(OTXT10)+' ||'+
035600120628     C                                   '§§Riga11§§'+'=='+%trim(OTXT11)+' ||'+
035700120628     C                                   '§§Riga12§§'+'=='+%trim(OTXT12)+' ||'+
035800120628     C                                   '§§Riga13§§'+'=='+%trim(OTXT13)+' ||'+
035900120628     C                                   '§§Riga14§§'+'=='+%trim(OTXT14)+' ||'+
036000120628     C                                   '§§Riga15§§'+'=='+%trim(OTXT15)+' ||'
036100120628     C*
036200120628     C                   ENDSR
036300120628     C***************************************************************
036400040524
036500040524
036600040524     C****************************************************************
036700040524     C* INIZIALIZZA LA VIDEATA CON I VALORI D DEFAULT
036800040524     C****************************************************************
036900040524     C     INZVID        BEGSR
037000060711     C*
037100060711     C                   CLEAR                   ST63DW01
037200040524     C*
037300040524     C                   ENDSR
037400040524     C***************************************************************
037500040217
037600040217
037700040217     C****************************************************************
037800040217     C     CHKINPUT      BEGSR
037900040217     C****************************************************************
038000040224     C*
038100040224     C                   CLEAR                   MSGERR
038200040217     C*
038300040217     C* Inizializzo il flag d controllo input
038400040217     C                   MOVEL     'S'           FlgOK             1
038500041013     C*
038600041013     C* Spengo gli indicatori d errore a video
038700060711     C                   SETOFF                                       313233
038800041013     C*
038900041013     C* Effettuo controllo su reperimento con successo record 'TRUL83R' in tabella 'MRA'
039000041013     C                   IF        *IN60 = *on
039100060711     C                   SETON                                        313233
039200041013     C                   MOVEL     'N'           FlgOK
039300060711     C                   EVAL      MSGERR = 'Record TIST63R non trovato in ' +
039400041013     C                                      'tabella MRA, avvisare il CED.'
039500041013     C                   ENDIF
039600040217     C*
039700040217     C* Effettuo controlli obbligatorietà parametri a video
039800060904     C                   IF        FlgOK = 'S'
039900060904     C                   IF        VIDPO  = *zeros
040000060904     C                   SETON                                        31
040100060904     C                   MOVEL     'N'           FlgOK
040200060904     C                   EVAL      MSGERR = 'Campo obbligatorio'
040300060904     C                   ENDIF
040400060904     C                   ENDIF
040500040224     C                   IF        FlgOK = 'S'
040600060711     C                   IF        VIDOBJ = *blanks
040700041005     C                   SETON                                        32
040800041005     C                   MOVEL     'N'           FlgOK
040900041005     C                   EVAL      MSGERR = 'Campo obbligatorio'
041000041005     C                   ENDIF
041100041005     C                   ENDIF
041200041005     C                   IF        FlgOK = 'S'
041300060711     C                   IF        VIDTXT1+VIDTXT2+VIDTXT3+VIDTXT4+VIDTXT5 +
041400060711     C                             VIDTXT6+VIDTXT7+VIDTXT8+VIDTXT9+VIDTXT10+
041500060711     C                             VIDTXT11+VIDTXT12+VIDTXT13+VIDTXT14+VIDTXT15=
041600060711     C                             *blanks
041700041005     C                   SETON                                        33
041800041005     C                   MOVEL     'N'           FlgOK
041900041005     C                   EVAL      MSGERR = 'Campo obbligatorio'
042000041005     C                   ENDIF
042100041005     C                   ENDIF
042200040217     C*
042300040217     C                   ENDSR
042400040217     C****************************************************************
042500060711
042600060711
042700060711
042800060711     C****************************************************************
042900060711     C* IMPOSTA I PARAMETRI X L'ESTRAZIONE ANAGRAFICO CLIENTI
043000060711     C****************************************************************
043100060711     C     IMPPARPTL     BEGSR
043200060711     C*
043300060711     C                   CLEAR                   PARAM
043400060711     C*
043500060711     C* REPERISCO L'ULTIMA DATA SALDI CLIENTE
043600060711     C                   Z-ADD     1             tblKUT
043700060711     C                   MOVEL     '5S'          tblCOD
043800060711     C                   MOVEL     *BLANKS       tblKEY
043900060711     C*
044000060711     C     KEYTABEL      CHAIN     TABEL00F                           30
044100060711     C                   IF        *IN30 = *ON OR TBLFLG <> ' '
044200060711     C                   Z-ADD     DATCOR        PARDAF
044300060711     C                   ELSE
044400060711     C                   EVAL      DS5S = TBLUNI
044500060711     C                   Z-ADD     §5SDAC        PARDAF
044600060711     C                   ENDIF
044700060711     C*
044800060711     C* Sottraggo 1 mese dalla data finale (ultimi saldi estratti)
044900060711     C     *ISO          MOVE      PARDAF        wDataIso
045000060714     C     wDataIso      SUBDUR    2:*M          wDataIso
045100060711     C     *ISO          MOVE      wDataIso      PARDAI
045200060711     C*
045300060711     C                   MOVEL     *blanks       PARKSC
045400060711     C                   MOVEL     *blanks       PARBLK
045500060711     C                   MOVEL     'S'           PARTBC
045600060711     C                   MOVEL     'N'           PARRGR
045700060711     C                   MOVEL     'N'           PAROVR
045800060711     C                   MOVEL     'S'           PARCHK
045900060711     C                   MOVEL     'S'           PARSOGN
046000060711     C                   Z-ADD     VIDPO         PARFIL
046100060711     C                   MOVEL     *blanks       PARSOC
046200060711     C                   MOVEL     'C'           PARCOF
046300060711     C                   MOVEL     '999'         PARNAZ
046400060711     C                   MOVEL     *blanks       PARCAP
046500060711     C                   MOVEL     *blanks       PARPRV
046600060714     C                   Z-ADD     250           PARFAT
046700060711     C                   MOVEL     'N'           PARCPI
046800060711     C                   MOVEL     'S'           PARBAR
046900060711     C                   MOVEL     'S'           PARSDI
047000060711     C                   MOVEL     'N'           PARPIC
047100060714     C                   MOVEL     'S'           PARFOA
047200060711     C                   MOVEL     '06'          PARNTE1
047300060711     C                   MOVEL     '03'          PARNTE2
047400060711     C                   MOVEL     '88'          PARNTE3
047500060711     C                   MOVEL     '85'          PARNTE4
047600060711     C                   MOVEL     '84'          PARNTE5
047700060711     C                   MOVEL     '87'          PARNTE6
047800060711     C                   MOVEL     '08'          PARNTE7
047900060711     C                   MOVEL     *blanks       PARNTE8
048000060711     C                   MOVEL     'S'           PARIND
048100060711     C                   MOVEL     'M'           PARRAGR
048200060711     C*
048300060711     C                   ENDSR
048400060711     C***************************************************************
048500040217
048600040217
048700040217     C****************************************************************
048800040217     C* REPERISCE I DATI UTENTE
048900040217     C****************************************************************
049000040217     C     REPDATIUTE    BEGSR
049100040217     C*
049200040217     C* INIZIALIZZA VARIABILI DI WRK
049300040217     C                   CLEAR                   TIBS34DS
049400040217     C                   CLEAR                   AZUTEDS
049500040217     C                   CLEAR                   DDATIUTE
049600040217     C*
049700040217     C     *DTAARA       DEFINE    §azute        azuteds
049800040217     C     *DTAARA       DEFINE    §datiute      ddatiute
049900040217     C                   IN(E)     *DTAARA
050000040217     C                   IF        %Error
050100040217     C                   EVAL      I34Tla = 'L'
050200040217     C                   CALL      'TIBS34R'
050300040217     C                   PARM                    Tibs34Ds
050400040217     C                   IN        *DTAARA
050500040217     C                   ENDIF
050600040217     C*
050700040217     C                   ENDSR
050800040217     C***************************************************************
050900041013
051000041013
051100041013
051200041013     C****************************************************************
051300041013     C* CARICA DATI TABELLATI
051400041013     C****************************************************************
051500041013     C     CARTAB        BEGSR
051600041013     C*
051700041013     C* Reperimento tabella MRA
051800041013     C                   CLEAR                   DMRA
051900041013     C                   MOVE(P)   'MRA'         tbeCOD
052000060711     C                   MOVEL(p)  'TIST63R'     tbeKE1
052100041013     C                   MOVE      *blanks       tbeKE2
052200041013     C     KEYTBE01P     CHAIN     TNTBE01L
052300041013     C                   IF        %found(TNTBE01L) AND
052400041013     C                             tbeATB <> 'A'
052500041013     C                   SETOFF                                       60
052600041013     C                   MOVEL     tbeUNI        DMRA
052700041013     C                   ELSE
052800041013     C                   SETON                                        60
052900120628     C***                EVAL      Esito = '2'
053000060711     C                   EVAL      MSGERR    = 'Record TIST63R non trovato in '+
053100051025     C                                      'tabella MRA, avvisare il CED.'
053200041013     C                   ENDIF
053300120628     C*
053400120628     C* Reperimento tabella MRABYPS
053500120628     C                   CLEAR                   InDMRABYPS
053600120628     C                   MOVE(P)   'MRA'         tbeCOD
053700120628     C                   MOVEL(p)  'TRBMBYPSR'   tbeKE1
053800120628     C                   MOVEL(p)  'HT1'         tbeKE2
053900120628     C     KEYTBE01P     CHAIN     TNTBE01L
054000120628     C                   IF        %found(TNTBE01L) AND
054100120628     C                             tbeATB <> 'A'
054200120628     C                   SETOFF                                       60
054300120628     C                   MOVEL     tbeUNI        InDMRABYPS
054400120628     C                   ELSE
054500120628     C                   SETON                                        60
054600120628     C                   EVAL      MsgErr = 'Record HT1 non trovato in'+
054700120628     C                                      ' tabella MRABYPS, avvisare il CED.'
054800120628     C                   ENDIF
054900041013     C*
055000041013     C                   ENDSR
055100041013     C***************************************************************
055200040217
055300040217
055400040217     C***************************************************************
055500040130     C     *INZSR        BEGSR
055600040217     C***************************************************************
055700040217     C*
055800041006     C     *ENTRY        PLIST
055900060711     C                   PARM                    KPJBA
056000040524     C*
056100040524     C* Reperisco il P.O. dell'utente corrente
056200040524     C                   EXSR      REPDATIUTE
056300060711     C*
056400060711     C* CALCOLA LA DATA CORRENTE
056500060711     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
056600060711     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
056700060711     C                   Z-ADD     WN8           G08DAT
056800060711     C                   Z-ADD     *ZEROS        G08INV
056900060711     C                   MOVEL     '0'           G08ERR
057000060711     C                   CALL      'XSRDA8'
057100060711     C                   PARM                    WLBDA8
057200060711     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
057300040218     C*
057400060711     C* CHIAVI DI LETTURA
057500060711     C* TNTBE01L
057600041013     C     KEYTBE01P     KLIST
057700041013     C                   KFLD                    tbeCOD
057800041013     C                   KFLD                    tbeKE1
057900041013     C                   KFLD                    tbeKE2
058000060711     C* TABEL00F
058100060711     C     KEYTABEL      KLIST
058200060711     C                   KFLD                    tblKUT
058300060711     C                   KFLD                    tblCOD
058400060711     C                   KFLD                    tblKEY
058500040217     C*
058600040130     C                   ENDSR
058700041006     O*------------------------------------------------------------------------*
058800041006     O* STAMPA
058900041006     O*------------------------------------------------------------------------*
059000060712     OPRTF198   E            STAFIN      1
059100060712     O                       RigaStampa
