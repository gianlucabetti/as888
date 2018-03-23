000100071128     ***********************************************************************************************
000200071128     **
000300071128     **
000400110615     ** PARAMETRI DELLA SOTTOPROCEDURA BartMail_Insert
000500071128     **
000600071128     ** Posizione Uso Tipo      Descrizione
000700071128     ** --------- --- --------- ------------------------------
000800110615     ** 1         IN  CHARACTER Tags
000900110616     ** 2         IN  CHARACTER DS di procedura DMRABYPS
001000110616     ** 3         IN  CHARACTER DS di procedura TRTCM1DS
001100110620     ** 4         IN  NUMERIC   Lunghezza singolo "tag" (totale)
001200110620     ** 5         OUT INTEGER   Esito
001300071128     ** --------- --- --------- ------------------------------
001400071129     **
001500071128     **
001600071128     ** ESEMPI DI CHIAMATA
001700071128     **
001800110616     ** if BartMail_Insert(pInTags:pInDMRABYPS:pInTRTCM1DS:pOutEsito) = 0
001900071128     **
002000071128     ** ISTRUZIONI PER LA COMPILAZIONE
002100071128     **
002200110615     ** 1. Creare il modulo TRBMBYPSR (Opz 15 PDM)
002300110615     ** 2. Creare/Aggiornare il programma di servizio TRBMBYPSR (CRTSRVPGM / UPDSRVPGM)
002400110615     ** 3. Creare il programma TRBMBYPSR dal modulo (CRTPGM) - solo se struttura anche *pgm (*ENTRY)
002500080610     ** 4. Cancellare il modulo.
002600071128     **
002700071128     ***********************************************************************************************
002800071128
002900071128      //********************************************************************************************
003000071128      //
003100071128      // Definizione prototipi procedure.
003200071128      //
003300071128      //********************************************************************************************
003400110615     D/COPY GAITRASRC/SRCPROTOPR,TMPNAM
003500110615     D/COPY GAITRASRC/SRCPROTOPR,UBRTVNETA
003600110615     D/COPY GAITRASRC/SRCPROTOPI,UBRTVNETA
003700110616     D/COPY GAITRASRC/SRCPROTOPR,TRBMBYPSR
003800071128
003900071128      //********************************************************************************************
004000071128      //
004100071128      // Definizione variabili globali.
004200071128      //
004300071128      //********************************************************************************************
004400110616     D InTags          S          65535A   VARYING
004500110620     D InLungh         S              5S 0
004600110616     D OutEsito        S             10I 0
004700110616
004800110616     D* Informazioni sul job
004900110616     D psds           sds
005000110616     D  psuser               254    263
005100110616
005200071127
005300071128      //********************************************************************************************
005400071128      //
005500071128      // Dichiarazione parametri.
005600071128      //
005700071128      //********************************************************************************************
005800071128     C     *ENTRY        PLIST
005900110616     C                   PARM                    InTags
006000110616     C                   PARM                    InDMRABYPS
006100110616     C                   PARM                    InTRTCM1DS
006200110620     C                   PARM                    InLungh
006300110616     C                   PARM                    OutEsito
006400071129
006500071128      /FREE
006600110620
006700110615
006800110620        if BartMail_Insert(InTags:InDMRABYPS:InTRTCM1DS:InLungh:OutEsito) = 0;
006900110616        else;
007000110616          OutEsito = -1;
007100110616        endif;
007200071129
007300071129
007400071129       return;
007500071128      /END-FREE
007600071128
007700071127     P*--------------------------------------------------
007800110615     P* Procedure name: BartMail_Insert
007900110615     P* Purpose:        Alimenta BartMail by-passando lo spool
008000110615     P* Returns:        Integer
008100110615     P* Parameter:      pInTags       => Elenco dei tags e dei relativi valori
008200110616     P* Parameter:      pInDMRABYPS   => DS di procedura
008300110615     P* Parameter:      pInTRTCM1DS   => DS di procedura
008400110620     P* Parameter:      pInLungh      => Lunghezza singolo "tag" (totale)
008500110615     P* Parameter:      pOutEsito     => Esito
008600071127     P*--------------------------------------------------
008700110615     P BartMail_Insert...
008800071129     P                 B                   EXPORT
008900071129     D*
009000110615     D BartMail_Insert...
009100071129     D                 PI            10I 0
009200110616     D  pInTags                   65535A   CONST VARYING
009300110616     D  pInDMRABYPS                        CONST LIKEDS(InDMRABYPS)
009400110616     D  pInTRTCM1DS                              LIKEDS(InTRTCM1DS)
009500110620     D  pInLungh                      5S 0 CONST
009600110615     D  pOutEsito                    10I 0 OPTIONS(*NOPASS)
009700071129
009800071129      //********************************************************************************************
009900071129      //
010000071129      // Definizione variabili work.
010100071129      //
010200071129      //********************************************************************************************
010300110615     D wStringaSQL     S           1024A   VARYING
010400110620     D wFullFilNam     S             16A   INZ
010500110620     D wFileName       S             10A   INZ
010600110620     D currSysNeta     S              8A   INZ('*NULL')
010700110620     D wTagLen         S              5S 0 INZ
010800110628     D wStdApi         S              1A   INZ('''')
010900110628     D wNewApi         S              1A   INZ('`')
011000110615
011100110615     D*------------------
011200110615     D* VARIABILI X LA CHIAMATA AL *PGM UTILITA' "XSPLIT"
011300110615     D*------------------
011400110616     D InStringa       S          65535A   VARYING                              (stringa input)
011500110615     D InSepara        S             10A                                        (separatore)
011600110615     D InTypeOut       S              1A                                        (tipo output)
011700110617     D wSkParam        S          65535A                                        (output)
011800110615     D OutErrore       S              1A                                        (flag errore)
011900110615     D OutMsg          S             80A                                        (messaggio errore)
012000110616     D SkSplitCSV_6    S            512    DIM(128) INZ
012100110616     D SkSplitCSV_6_2  S            512    DIM(128) INZ
012200071127
012300071129
012400071127      /FREE
012500110615        // Inizializzo esito a errore
012600110616        pOutEsito = -1;
012700110620
012800110620
012900110620        // Se nn esplicitata lunghezza singolo "tag" (totale) => imposto a 640
013000110620        wTagLen = pInLungh;
013100110620
013200110620        if wTagLen = 0;
013300110620           wTagLen = 640;
013400110620        endif;
013500110620
013600110620
013700110620        // Se impostata lunghezza eccessiva singolo "tag" (totale) => imposto al massimo
013800110620        if wTagLen > 32766;
013900110620           wTagLen = 32766;
014000110620        endif;
014100071129
014200110615
014300110615        //Reperisco un nome univoco
014400110620        wFullFilNam = %str(ProduceTemporaryFileName(*OMIT));
014500110620        wFileName = %subst(wFullFilNam : 7);
014600110615
014700071129
014800110615        // Reperisco il sistema corrente
014900110615        if UBRTVNETA_Rtv(currSysNeta) = 0;
015000110615
015100110615
015200071129        // Se tutto ok proseguo
015300110620        if wFullFilNam <> *blanks;
015400110620              wStringaSQL = 'create table ' + %trim(wFullFilNam) +
015500110620                            ' (DATI CHAR ( ' + %char(wTagLen) +
015600110620                            ' ) NOT NULL WITH DEFAULT) ';
015700110615              exec sql execute immediate :wStringaSQL;
015800110615
015900110615              if sqlcode = *zeros;
016000110620
016100110620              wStringaSQL = 'insert into ' + %trim(wFullFilNam) +
016200110620                            ' (DATI) VALUES(''1'')';
016300110620              exec sql execute immediate :wStringaSQL;
016400110620
016500110620              if sqlcode = *zeros;
016600110615
016700110615      /END-FREE
016800110615
016900110620     C                   eval      wStringaSQL = 'insert into '           +
017000110620     C                             %trim(wFullFilNam) + ' (DATI) VALUES(''' +
017100110620     C                             %trim(SkSplitCSV_6_2(1)) + '#'         +
017200110620     C                             %trim(SkSplitCSV_6_2(2)) + '}{'')'
017300110615     C*
017400110615     C                   eval      InStringa = pInTags
017500110617     C                   eval      wSkParam  = ''
017600110615     C*
017700110615     C* Chiamo il *pgm d utilità x lo "split" dei tags
017800110615     C                   CALL(e)   'XSPLIT2'
017900110615     C                   PARM                    InStringa
018000110615     C                   PARM      '||'          InSepara
018100110616     C                   PARM      '6'           InTypeOut
018200110617     C                   PARM                    wSkParam
018300110615     C                   PARM                    OutErrore
018400110615     C                   PARM                    OutMsg
018500110616     C                   MOVEA     wSkParam      SkSplitCSV_6
018600110615     C*
018700110615     C* Se errore in chiamata "splittatore"
018800110615     C                   if        not %error
018900110615     C*
019000110615     C* Sotto-splitto ogni singolo elemento della schiera risultante
019100110615     C                   z-add     1             i                 4 0
019200110616     C                   dow       i <= %elem(SkSplitCSV_6)
019300110616     C                   if        SkSplitCSV_6(i) = *blanks
019400110615     C                   leave
019500110615     C                   else
019600110615     C*
019700110616     C                   eval      InStringa = SkSplitCSV_6(i)
019800110617     C                   eval      wSkParam  = ''
019900110615     C*
020000110615     C* Chiamo il *pgm d utilità x lo "split" del singolo tags
020100110615     C                   CALL(e)   'XSPLIT2'
020200110615     C                   PARM                    InStringa
020300110615     C                   PARM      '=='          InSepara
020400110616     C                   PARM      '6'           InTypeOut
020500110617     C                   PARM                    wSkParam
020600110615     C                   PARM                    OutErrore
020700110615     C                   PARM                    OutMsg
020800110616     C                   MOVEA     wSkParam      SkSplitCSV_6_2
020900110615     C*
021000110616     C* Se no errore in chiamata "splittatore" procedo
021100110615     C                   if        not %error
021200110628     C
021300110628      /FREE
021400110628
021500110628        SKSPLITCSV_6_2(2) =
021600110628                  %xlate(wStdApi:wNewApi:SKSPLITCSV_6_2(2));
021700110628
021800110628      /END-FREE
021900110628     C
022000110615     C*
022100110617     C                   eval      wStringaSQL = 'insert into '           +
022200110620     C                             %trim(wFullFilNam) + ' (DATI) VALUES(''  ' +
022300110616     C                             %trim(SkSplitCSV_6_2(1)) + '#'         +
022400110616     C                             %trim(SkSplitCSV_6_2(2)) + '}{'')'
022500110615     C
022600110615      /FREE
022700110615              exec sql execute immediate :wStringaSQL;
022800110615              if sqlcode = *zeros;
022900110615              else;
023000110616                 return pOutEsito;
023100110615              endif;
023200110615      /END-FREE
023300110615     C                   endif
023400110615     C                   endif
023500110615     C                   add       1             i
023600110615     C                   enddo
023700110615     C*
023800110615     C                   endif
023900110615     C*
024000110615     C* Effettuo lancio XSPLIT solo x chiusura
024100110615     C                   CALL(e)   'XSPLIT2'
024200110616     C                   PARM      ''            InStringa
024300110615     C                   PARM                    InSepara
024400110615     C                   PARM                    InTypeOut
024500110616     C                   PARM      ''            wSkParam
024600110615     C                   PARM                    OutErrore
024700110615     C                   PARM                    OutMsg
024800110620     C*
024900110620     C* Valorizzo i dati siognificativi necessari ai driver
025000110620     C                   EVAL      FILNAM  = pInDMRABYPS.§MRASPLNAM
025100110620     C                   EVAL      JOBNAM  = 'ZZZ'+pInTRTCM1DS.§CM1PROG
025200110620     C                   EVAL      USRNAM  = psuser
025300110620     C                   EVAL      JOBNUM  = *all'9'
025400110620     C                   EVAL      NUMFIL  = *all'9'
025500110620     C                   EVAL      JOBSYSN = currSysNeta
025600110620     C                   EVAL      USRDTA  = pInDMRABYPS.§MRAUSRDTA
025700110620     C                   EVAL      PRTTYP  = '*FILEIFS'
025800110620     C*
025900110620     C                   EVAL      wPATHIFS = '/BARTMAILING/'
026000110620     C                   EVAL      wFILIFS  = %trim(FILNAM)+
026100110620     C                                        %trim(JOBNAM)+
026200110620     C                                        %trim(USRNAM)+
026300110620     C                                        %trim(JOBNUM)+
026400110620     C                                        %trim(NUMFIL)
026500110620     C                   EVAL      wEXTIFS  = '.TXT'
026600110620     C*
026700110620     C* Pubblico il file nel IFS
026800110620     C                   CALL(e)   'TRBMF2IFC'
026900110620     C                   PARM      'QTEMP'       wLIBIN           10
027000110620     C                   PARM      wFileName     wFILIN           10
027100110620     C                   PARM      *blanks       wMBRIN           10
027200110620     C                   PARM      '*'           wFLDSELIN        20
027300110620     C                   PARM      *blanks       wNOMEALIAS       22
027400110620     C                   PARM                    wPATHIFS        255
027500110620     C                   PARM                    wFILIFS         255
027600110620     C                   PARM                    wEXTIFS         255
027700110620     C                   PARM      wTagLen       wLUNGH            5 0
027800110620     C                   PARM      'Y'           wOUTTRIM          1
027900110620     C                   PARM      '*CRLF'       wENDLIN           6
028000110620     C                   PARM      01252         wCCCSD            5 0
028100110620     C                   PARM      *blanks       wMESSAGGIO       80
028200110620     C                   PARM      '0'           wESITO            1
028300110620     C*
028400110620     C* Se a questo punto tutto ok => proseguo scivendo il log pilota
028500110620     C                   IF        not %error
028600110620     C*
028700110620     C                   IF        wESITO = '2'
028800110620     C                   EVAL      MESSAGGIO = 'TRBMF2IFC - Errore durante ' +
028900110620     C                                'esportazione dati nel IFS'
029000110620     C                   EVAL      ESITO     = '2'
029100110620     C                   ELSE
029200110620     C                   EVAL      ESITO     = '0'
029300110620     C                   ENDIF
029400110615     C*
029500110615     C* Inserisco il record nel file di log Mailing Automatico
029600110615     C                   CALL(e)   'TRTCM1R1'
029700110615     C                   PARM                    FILNAM           10
029800110615     C                   PARM                    JOBNAM           10
029900110615     C                   PARM                    USRNAM           10
030000110615     C                   PARM                    JOBNUM            6
030100110615     C                   PARM                    NUMFIL            6
030200110615     C                   PARM                    JOBSYSN           8
030300110615     C                   PARM                    USRDTA           10
030400110615     C                   PARM      1             WTPAG             9 0
030500110615     C                   PARM      999           PRTFLEN          10 0
030600110615     C                   PARM      999           SPLCPI           10 0
030700110616     C                   PARM                    pInTRTCM1DS
030800110616     C                   PARM                    PRTTYP           10
030900110616     C                   PARM                    MESSAGGIO        80
031000110616     C                   PARM                    ESITO             1
031100110615     C*
031200110615     C* Se a questo punto tutto ok => chiamata corretta
031300111031     C                   IF        not %error AND ESITO <> '2'
031400110616     C                   EVAL      pOutEsito = 0
031500110615     C                   ENDIF
031600110620     C*
031700110620     C                   ENDIF
031800110615     C*
031900110615      /FREE
032000110620              wStringaSQL = 'drop table ' + %trim(wFullFilNam);
032100110616              exec sql execute immediate :wStringaSQL;
032200110615
032300110615              endif;
032400071129        endif;
032500110615        endif;
032600110620        endif;
032700071129
032800071127
032900110616        return pOutEsito;
033000071127
033100071127      /END-FREE
033200071129
033300110615     P BartMail_Insert...
033400071129     P                 E
033500110615
