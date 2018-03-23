000100060801      *===============================================================*
000200061127     H decedit('0,') datedit(*dmy/)
000300061127     F*------------------------------------------------------------------------*
000400061127     F* DATA BASE
000500061127     F*------------------------------------------------------------------------*
000600061127     Fazcpr00f  IF   E             DISK
000700061127     Fazdst00f  UF   E             DISK    prefix(d_)
000800061127     Fazdstp0f  UF   E             DISK    prefix(p_)
000900061127     Fazdss00f  UF   E             DISK    prefix(s_)
001000061127     FPRTF198   O    F  220        PRINTER usropn
001100061127     D*-------------------
001200061127     D* DS ARCHITETTURA
001300061127     D*-------------------
001400061127     D KPJBA         e ds
001500061127     D*-------------------
001600061127     D* DS CALCOLO INSTRADAMENTO
001700061127     D*-------------------
001800061127     D TISI95DS      e ds
001900080617     D*-------------------
002000080617     D* PROGRAM STATUS DS
002100080617     D*-------------------
002200080617     D psds           sds
002300080617     D  procname         *PROC
002400080617     D JOB_NAME              244    253
002500080617     D USER                  254    263
002600080617     D JOB_NUM               264    269  0
002700061127     D*------------------
002800061127     D* DS "XSRDA8" - INVERSIONE DATA (8)
002900061127     D*------------------
003000061127     D WLBDA8          DS                  INZ
003100061127     D  G08DAT                 1      8  0
003200061127     D  G08INV                 9     16  0
003300061127     D  G08ERR                17     17
003400061127     D  G08TGI                18     22  0
003500061127     D*-------------------
003600061127     D* DS RIDEFINIZIONE PARAMETRI DI INPUT
003700061127     D*-------------------
003800061127     D DS_Input        ds
003900061127     D  In_Simula                     1
004000061129     D  In_DataRif                    8
004100061129     D*-------------------
004200061129     D* VARIABILI D WRK
004300061129     D*-------------------
004400061129     D wPrtTesta       s              4    inz
004500080617     D wPrtError       s              1    inz
004600061129     D wDataRif        s              8  0 inz
004700061127     C*------------------------------------------------------------------------*
004800061127     C* MAIN LINE
004900061127     C*------------------------------------------------------------------------*
005000061127     C*
005100061127     C     *Entry        plist
005200061127     C                   parm                    KPJBA
005300061127     C                   movel     KPJBU         DS_Input
005400061129     C*
005500061129     C                   move(p)   In_DataRif    wDataRif
005600061127     C*
005700061127     C* Se passata data elabora direttamente con quella data
005800061129     C                   if        wDataRif > *zeros
005900061127     C                   exsr      Procedi
006000061127     C*
006100120302     C* Altrimenti reperisco la data corrente e verifico nel file versioni cappario BRT
006200061127     C* se c'è una versione con data decorrenza = data corrente
006300061127     C* => se esiste elaboro
006400061129     C                   else
006500061129     C*
006600061129     C* Se lanciato in modalità diciamo "automatica" forzo il parametro d simulazione in
006700061129     C* aggiornamento
006800061129     C                   eval      In_Simula = 'A'
006900061127     C*
007000061127     C* Calcolo da data corrente
007100061127     C                   time                    WN14             14 0          *ORA (6)+ DATA (8)
007200061127     C                   move      WN14          WN8               8 0          *DATA (8) IN G/M/AA
007300061127     C                   z-add     WN8           G08DAT
007400061127     C                   z-add     *ZEROS        G08INV
007500061127     C                   movel     '0'           G08ERR
007600061127     C                   call      'XSRDA8'
007700061127     C                   parm                    WLBDA8
007800061127     C                   z-add     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
007900061127     C*
008000061127     C     *start        setll     azcpr00f
008100061127     C                   read      azcpr00f
008200061127     C                   dow       not %eof(azcpr00f)
008300061127     C                   if        cprATB = *blanks
008400061127     C                   if        cprDDE = DATCOR
008500061127     C                   exsr      Procedi
008600061127     C                   leave
008700061127     C                   endif
008800061127     C                   endif
008900061127     C                   read      azcpr00f
009000061127     C                   enddo
009100061127     C*
009200061127     C                   endif
009300061127     C*
009400061127     C* Effettuo chiamata in sola chiusura TISI95R
009500061127     C                   clear                   TISI95DS                       *azzera i parametri
009600061127     C                   movel     'C'           I95TLA                         *tipo lancio
009700061127     C                   call      'TISI95R'
009800061127     C                   parm                    TISI95DS
009900061127     C*
010000061127     C* Fine
010100061127     C                   eval      *INLR = *on
010200061127     C*--------------------------------------------------------------------------------------------*
010300061127     C* Procedi - Routine iter di elaborazinoe
010400061127     C*--------------------------------------------------------------------------------------------*
010500061127     C     Procedi       BEGSR
010600080617     C*
010700080617     C                   setoff                                       44
010800061127     C*
010900061127     C* Elabora le anagrafiche destinatari particolari
011000061127     C                   exsr      exeDst
011100061127     C*
011200061127     C* Elabora le proposte destinatari particolari
011300061127     C                   exsr      exeDstP
011400061127     C*
011500061127     C* Elabora i sinonimi destinatari particolari
011600061127     C                   exsr      exeDss
011700080617     C*
011800080617     C* Se occorsi errori da gestire a mano => mando mail d segnalazione
011900080617     C                   if        *in44 = *on
012000080617     C                   exsr      sndEML
012100080617     C                   endif
012200061127     C*
012300061127     C                   ENDSR
012400061127     C*--------------------------------------------------------------------------------------------*
012500061127     C* exeDst - Elaborazione anagrafiche destinatari particolari
012600061127     C*--------------------------------------------------------------------------------------------*
012700061127     C     exeDst        BEGSR
012800061129     C*
012900061129     C* Imposto la descrizione testata in stampa
013000080617     C                   eval      wPrtTesta = 'DEST'
013100080617     C                   eval      wPrtError = *blanks
013200061127     C*
013300061127     C* Apro il printer file
013400061127     C                   open      PRTF198
013500061127     C*
013600061127     C* Scorro tutto il file AZDST
013700061127     C     *start        setll     azdst00f
013800061127     C                   read      azdst00f
013900061127     C                   dow       not %eof(azdst00f)
014000061127     C*
014100061127     C* X sicurezza elaboro solo le nazioni Italia
014200061127     C                   if        d_dstNAZ = *blanks
014300061127     C*
014400061127     C                   clear                   TISI95DS                       *azzera i parametri
014500061127     C                   movel     *blanks       I95TLA                         *tipo lancio
014600061127     C                   movel     '7'           I95TCN                         *tipo controllo
014700061127     C                   movel     d_dstNAZ      I95NAR                         *nazione
014800061127     C                   movel     d_dstCAP      I95CAP                         *cap
014900061127     C                   movel     d_dstLOC      I95LOC                         *località
015000061127     C                   movel     d_dstPRV      I95PRV                         *provincia
015100061129     C                   z-add     wDataRif      I95DAT                         *data di riferimento
015200061127     C*
015300061127     C                   call      'TISI95R'
015400061127     C                   parm                    TISI95DS
015500061127     C*
015600061127     C                   setoff                                       55
015700061127     C*
015800061127     C* Verifico se TISI95R ha veramente "normalizzato" qualcosa
015900061127     C                   if        d_dstCAP <> O95CAP or
016000061127     C                             d_dstPRV <> O95PRV or
016100061127     C                             d_dstLOC <> O95LOC
016200061127     C*
016300061127     C* Se livello dati 3 o 4 "normalizzo" CAP/LOC/PRV
016400061127     C                   if        O95LID = '3' or
016500061127     C                             O95LID = '4'
016600061127     C                   except    staUpdDst
016700061127     C                   eval      d_dstCAP = O95CAP
016800061127     C                   eval      d_dstPRV = O95PRV
016900061127     C                   eval      d_dstLOC = O95LOC
017000061127     C                   seton                                        55
017100061127     C                   endif
017200061127     C                   endif
017300061127     C*
017400061127     C* Verifico se TISI95R ha veramente "normalizzato" qualcosa
017500061127     C                   if        d_dstPRV <> O95PRV
017600061127     C*
017700061127     C* Se livello dati 2 "normalizzo" solo PRV
017800061127     C                   if        O95LID = '2'
017900061127     C                   except    staUpdDst
018000061127     C                   eval      d_dstPRV = O95PRV
018100061127     C                   seton                                        55
018200061127     C                   endif
018300061127     C                   endif
018400061127     C*
018500061127     C* Se "normalizzato" qualcosa => aggiorno il record corrente (se no simulazione)
018600061127     C                   if        *in55     = *on AND
018700061127     C                             In_Simula = 'A'
018800061127     C                   eval      d_dstFTR = *blanks
018900090113     C                   update(e) azdst000
019000080617     C* Se errore gestisco e tengo traccia
019100080617     C                   if        %error
019200090113     C                   delete    azdst000
019300080617     C                   eval      wPrtError = 'E'
019400080617     C                   seton                                        44
019500080617     C                   endif
019600080617     C*
019700061127     C                   endif
019800061127     C*
019900061127     C                   endif
020000061127     C*
020100061127     C* Proseguo con la lettura del file AZDST
020200061127     C                   read      azdst00f
020300061127     C                   enddo
020400061127     C*
020500061127     C* Chiudo il printer file
020600061127     C                   close     PRTF198
020700061127     C*
020800061127     C                   ENDSR
020900061127     C*--------------------------------------------------------------------------------------------*
021000061127     C* exeDstP - Elaborazione proposte destinatari particolari
021100061127     C*--------------------------------------------------------------------------------------------*
021200061127     C     exeDstP       BEGSR
021300061129     C*
021400061129     C* Imposto la descrizione testata in stampa
021500061129     C                   eval      wPrtTesta = 'PROP'
021600061127     C*
021700061127     C* Apro il printer file
021800061127     C                   open      PRTF198
021900061127     C*
022000061127     C* Scorro tutto il file AZDSTP
022100061127     C     *start        setll     azdstp0f
022200061127     C                   read      azdstp0f
022300061127     C                   dow       not %eof(azdstp0f)
022400061127     C*
022500061127     C* X sicurezza elaboro solo le nazioni Italia
022600061127     C                   if        p_dstNAZ = *blanks
022700061127     C*
022800061127     C                   clear                   TISI95DS                       *azzera i parametri
022900061127     C                   movel     *blanks       I95TLA                         *tipo lancio
023000061127     C                   movel     '7'           I95TCN                         *tipo controllo
023100061127     C                   movel     p_dstNAZ      I95NAR                         *nazione
023200061127     C                   movel     p_dstCAP      I95CAP                         *cap
023300061127     C                   movel     p_dstLOC      I95LOC                         *località
023400061127     C                   movel     p_dstPRV      I95PRV                         *provincia
023500061129     C                   z-add     wDataRif      I95DAT                         *data di riferimento
023600061127     C*
023700061127     C                   call      'TISI95R'
023800061127     C                   parm                    TISI95DS
023900061127     C*
024000061127     C                   setoff                                       55
024100061127     C*
024200061127     C* Verifico se TISI95R ha veramente "normalizzato" qualcosa
024300061127     C                   if        p_dstCAP <> O95CAP or
024400061127     C                             p_dstPRV <> O95PRV or
024500061127     C                             p_dstLOC <> O95LOC
024600061127     C*
024700061127     C* Se livello dati 3 o 4 "normalizzo" CAP/LOC/PRV
024800061127     C                   if        O95LID = '3' or
024900061127     C                             O95LID = '4'
025000061127     C                   except    staUpdDstP
025100061127     C                   eval      p_dstCAP = O95CAP
025200061127     C                   eval      p_dstPRV = O95PRV
025300061127     C                   eval      p_dstLOC = O95LOC
025400061127     C                   seton                                        55
025500061127     C                   endif
025600061127     C                   endif
025700061127     C*
025800061127     C* Verifico se TISI95R ha veramente "normalizzato" qualcosa
025900061127     C                   if        p_dstPRV <> O95PRV
026000061127     C*
026100061127     C* Se livello dati 2 "normalizzo" solo PRV
026200061127     C                   if        O95LID = '2'
026300061127     C                   except    staUpdDstP
026400061127     C                   eval      p_dstPRV = O95PRV
026500061127     C                   seton                                        55
026600061127     C                   endif
026700061127     C                   endif
026800061127     C*
026900061127     C* Se "normalizzato" qualcosa => aggiorno il record corrente
027000061127     C                   if        *in55     = *on AND
027100061127     C                             In_Simula = 'A'
027200090113     C                   update(e) azdstp00
027300080617     C* Se errore gestisco e tengo traccia
027400080617     C                   if        %error
027500090113     C                   delete    azdstp00
027600080617     C                   eval      wPrtError = 'E'
027700080617     C                   seton                                        44
027800080617     C                   endif
027900080617     C*
028000061127     C                   endif
028100061127     C*
028200061127     C                   endif
028300061127     C*
028400061127     C* Proseguo con la lettura del file AZDSTP
028500061127     C                   read      azdstp0f
028600061127     C                   enddo
028700061127     C*
028800061127     C* Chiudo il printer file
028900061127     C                   close     PRTF198
029000061127     C*
029100061127     C                   ENDSR
029200061127     C*--------------------------------------------------------------------------------------------*
029300061127     C* exeDss - Elaborazione sinonimi destinatari particolari
029400061127     C*--------------------------------------------------------------------------------------------*
029500061127     C     exeDss        BEGSR
029600061129     C*
029700061129     C* Imposto la descrizione testata in stampa
029800061129     C                   eval      wPrtTesta = 'SIN'
029900061127     C*
030000061127     C* Apro il printer file
030100061127     C                   open      PRTF198
030200061127     C*
030300061127     C* Scorro tutto il file AZDSS
030400061127     C     *start        setll     azdss00f
030500061127     C                   read      azdss00f
030600061127     C                   dow       not %eof(azdss00f)
030700061127     C*
030800061127     C* X sicurezza elaboro solo le nazioni Italia
030900061127     C                   if        s_dssNAZ = *blanks
031000061127     C*
031100061127     C                   clear                   TISI95DS                       *azzera i parametri
031200061127     C                   movel     *blanks       I95TLA                         *tipo lancio
031300061127     C                   movel     '7'           I95TCN                         *tipo controllo
031400061127     C                   movel     s_dssNAZ      I95NAR                         *nazione
031500061127     C                   movel     s_dssCAP      I95CAP                         *cap
031600061127     C                   movel     s_dssLOC      I95LOC                         *località
031700061127     C                   movel     s_dssPRV      I95PRV                         *provincia
031800061129     C                   z-add     wDataRif      I95DAT                         *data di riferimento
031900061127     C*
032000061127     C                   call      'TISI95R'
032100061127     C                   parm                    TISI95DS
032200061127     C*
032300061127     C                   setoff                                       55
032400061127     C*
032500061127     C* Verifico se TISI95R ha veramente "normalizzato" qualcosa
032600061127     C                   if        s_dssCAP <> O95CAP or
032700061127     C                             s_dssPRV <> O95PRV or
032800061127     C                             s_dssLOC <> O95LOC
032900061127     C*
033000061127     C* Se livello dati 3 o 4 "normalizzo" CAP/LOC/PRV
033100061127     C                   if        O95LID = '3' or
033200061127     C                             O95LID = '4'
033300061127     C                   except    staUpdDss
033400061127     C                   eval      s_dssCAP = O95CAP
033500061127     C                   eval      s_dssPRV = O95PRV
033600061127     C                   eval      s_dssLOC = O95LOC
033700061127     C                   seton                                        55
033800061127     C                   endif
033900061127     C                   endif
034000061127     C*
034100061127     C* Verifico se TISI95R ha veramente "normalizzato" qualcosa
034200061127     C                   if        s_dssPRV <> O95PRV
034300061127     C*
034400061127     C* Se livello dati 2 "normalizzo" solo PRV
034500061127     C                   if        O95LID = '2'
034600061127     C                   except    staUpdDss
034700061127     C                   eval      s_dssPRV = O95PRV
034800061127     C                   seton                                        55
034900061127     C                   endif
035000061127     C                   endif
035100061127     C*
035200061127     C* Se "normalizzato" qualcosa => aggiorno il record corrente
035300061127     C                   if        *in55     = *on AND
035400061127     C                             In_Simula = 'A'
035500061127     C                   eval      s_dssFTR = *blanks
035600090113     C                   update(e) azdss000
035700080617     C* Se errore gestisco e tengo traccia
035800080617     C                   if        %error
035900090113     C                   delete    azdss000
036000080617     C                   eval      wPrtError = 'E'
036100080617     C                   seton                                        44
036200080617     C                   endif
036300080617     C*
036400061127     C                   endif
036500061127     C*
036600061127     C                   endif
036700061127     C*
036800061127     C* Proseguo con la lettura del file AZDSS
036900061127     C                   read      azdss00f
037000061127     C                   enddo
037100061127     C*
037200061127     C* Chiudo il printer file
037300061127     C                   close     PRTF198
037400061127     C*
037500061127     C                   ENDSR
037600080617     C*------------------------------------------------------------------------*
037700080617     C* SNDEML - COMPONE TESTO CORPO MESSAGGI OED INVIA E-MAIL
037800080617     C*------------------------------------------------------------------------*
037900080617     C     SNDEML        BEGSR
038000080617     C*
038100080617     C* Inizializzo variabili
038200080617     C                   movel     *blanks       wrkEml          253
038300080617     C                   movel     *blanks       wrkMsg         5000
038400080617     C                   movel     *blanks       wrkOgg           44
038500080617     C* Valorizzo i campi della e-m@ail
038600120302     C                   eval      wrkEml='fabrizio.gurrieri@brt.it ; ' +
038700120302     C                                    'cedalert@brt.it'
038800090113     C                   eval      wrkOgg='La normalizzazione DESTINATARI '   +
038900090113     C                                    'PARTICOLARI ha prodotto risultati '+
039000090113     C                                    'già esistenti. Le anagrafiche '    +
039100090113     C                                    '"doppie" sono state quindi '       +
039200090113     C                                    'eliminate'
039300090113     C                   eval      wrkMsg='Verificare gli spool del job:   :/N'+
039400080617     C                                                                    ':/N'+
039500080617     C                                                    JOB_NAME       +':/N'+
039600080617     C                                                    USER           +':/N'+
039700090113     C                                              %char(JOB_NUM)       +':/N'
039800080617     C*
039900080617     C                   CALL(e)   'TIS701C'
040000080617     C                   PARM                    wrkEml
040100080617     C                   PARM                    wrkOgg
040200080617     C                   PARM                    wrkMsg
040300080617     C*
040400080617     C                   ENDSR
040500061127     C*------------------------------------------------------------------------*
040600061127     OPRTF198   E            staUpdDst   1
040700061129     O                       wPrtTesta           +1
040800080617     O                       wPrtError           +1
040900061127     O                       d_dstPRV            +1
041000061127     O                       d_dstCAP            +1
041100061127     O                       d_dstLOC            +1
041200061127     O                       d_dstRAG            +1
041300061129     O                       O95PRV              +5
041400061127     O                       O95CAP              +1
041500061127     O                       O95LOC              +1
041600061127     O*------------
041700061127     OPRTF198   E            staUpdDstP  1
041800061129     O                       wPrtTesta           +1
041900080617     O                       wPrtError           +1
042000061127     O                       p_dstPRV            +1
042100061127     O                       p_dstCAP            +1
042200061127     O                       p_dstLOC            +1
042300061127     O                       p_dstRAG            +1
042400061129     O                       O95PRV              +5
042500061127     O                       O95CAP              +1
042600061127     O                       O95LOC              +1
042700061127     O*------------
042800061127     OPRTF198   E            staUpdDss   1
042900061129     O                       wPrtTesta           +1
043000080617     O                       wPrtError           +1
043100061127     O                       s_dssPRV            +1
043200061127     O                       s_dssCAP            +1
043300061127     O                       s_dssLOC            +1
043400061127     O                       s_dssRAG            +1
043500061127     O                       s_dssSIN            +1
043600061129     O                       O95PRV              +5
043700061127     O                       O95CAP              +1
043800061127     O                       O95LOC              +1
