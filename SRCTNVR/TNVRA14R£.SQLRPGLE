000100081209     H DECEDIT('0,') DATEDIT(*YMD.)
000200090109     H* TNVRA15R *---------------------------------------------------*
000300081110     H*                                                              *
000400081209     H* x aggiornare occorre passare il flag PARAMETRO a "S"         *
000500081209     H*  altrimenti esegue solo una stampa.                          *
000600081209     H*                                                              *
000700000000     H*--------------------------------------------------------------*
000800090109     Ffifgt00f  uF a E             DISK    rename(FIFGT000:FGTFisico)
000900081222     F                                     usropn
001000090109     Ffitgt00f  o  a E           K DISK
001100090109      *
001200090109     Ffifpt01L  iF a E           K DISK
001300090109     Ffifpd01L  iF a E           K DISK
001400081211      *
001500090401     Ffiapd01L  iF   E           K DISK
001600090401      *
001700081110     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
001800081211      * -------------------------------------------------------------
001900090421     d dapdflr       e ds
002000090421     dficn08ds       e ds
002100081110     D dsFGT         E DS                  ExtName(FIFGT00F)
002200081110     d                                     prefix(STP:3)
002300081110     D esito           s             10i 0
002400081110     D FGT_REC         s              9s 0
002500081230      *
002600090109     D KPJBA         E DS
002700090109      *
002800090109     D ficn05ds      E DS
002900090109     D dGPI          E DS
003000900521     C*****************************************************************
003100090112     C****+ dec( year(curdate())*10000 + month(curdate())*100 +
003200090112     C****+ day(curdate()) , 8, 0)
003300090112     C****+ between fgtddt and fgtdst
003400090112     C*****************************************************************
003500090422     C*
003600090109     c                   clear                   savSMLpdr         8
003700081209     C                   SETON                                        LR
003800090422     C*
003900090422     C*=================================================================
004000020418     C*
004100090422     C* Prima fa scadere tutte quelle tariffe che non hanno Autisti valorizzati
004200090422     C*  e non valorizzati nel 2009 mettendo la data scadenza tariffa "20090430"
004300090422     C*
004400090422     C*=================================================================
004500090422     C*
004600090422     C*   TARIFFE  DA  FAR  SCADERE  PRIMA   DELLA CONVERSIONE
004700090422     C*
004800090422     C*  Aggiorna la data SCADENZA al 20090430 di tutte le tariffe di quegli
004900090422     C*   AUT senza alcuna VALORIZZAZIONE in assoluto
005000090422     C*      ========
005100090422     C/EXEC SQL
005200090422     C+ update fifgt00f set fgtdst = 20090430
005300090422     C+ where fgtdst > 20090430
005400090422     C+ and (fgtddt < 20090401 or fgtddt > 20091231)
005500090422     C+ and fgttsr in ('C', 'R', 'G') and
005600090422     C+ fgtpdr NOT in (select fttpdr
005700090422     C+ from fiftt00f group by fttpdr )
005800090422     C/END-EXEC
005900090422     C*
006000090422     C*  Aggiorna la data SCADENZA al 20090430 di tutte le tariffe di quegli
006100090422     C*  AUT presenti sulle valorizzazioni ma che NON hanno Valorizzazioni nel 2009.
006200090422     C*      ========
006300090422     C/EXEC SQL
006400090422     C+ update fifgt00f set fgtdst = 20090430
006500090422     C+ where fgtdst > 20090430 and fgttsr in ('C', 'R', 'G')
006600090422     C+ and (fgtddt < 20090401 or fgtddt > 20091231)
006700090422     C+ and fgtpdr in (select fttpdr from fiftt00f
006800090422     C+ where fttddc <=20081231 and fttpdr NOT
006900090422     C+ in (select fttpdr from fiftt00f WHERE
007000090422     C+ fttddc > 20081231) group by fttpdr )
007100090422     C/END-EXEC
007200090422     C*=================================================================
007300090422      **
007400090422      **
007500090422      **
007600090422      **
007700090422      **  QUINDI DEVE leggere tutte le tariffe AUTISTI (C,R,G) con data decorrenza
007800090422      **  minore del 2010 e con Data SCADENZA superiore al 1°maggio 2009
007900090422      **      Queste sono le tariffe da CONVERTIRE creando la SUPERTESTATA
008000090422      *
008100090422      **  Ricordando che PRIMA sono state fatte scadere al 30/04/2009 tutte le altre
008200090422      *
008300090422     C/EXEC SQL DECLARE A1 CURSOR FOR
008400090422     C+ SELECT rrn(fifgt00f), fifgt00f.* FROM
008500090422     C+ fifgt00f WHERE  fgttsr in ('C', 'R', 'G') and
008600090422     C+  fgtddt < 20100101 and fgtdst >= 20090501
008700090422     C+ ORDER BY FGTSML, FGTPDR, FGTTSR, FGTCTR, FGTPRG
008800090422     C/END-EXEC
008900081110     C* Segnalare l'errore in qualche modo, p.e. stampando il JOBLOG
009000081110     c                   if        sqlCODE < 0
009100081110     c                   eval      esito = Sqlcode
009200081110     c                   dump(a)
009300081110     c                   return
009400081110     c                   end
009500081110     C/EXEC SQL
009600081110     C+ OPEN A1
009700081110     C/END-EXEC
009800081110     C* Segnalare l'errore in qualche modo, p.e. stampando il JOBLOG
009900081110     c                   if        sqlCODE < 0
010000081110     c                   eval      esito = Sqlcode
010100081110     c                   dump(a)
010200081110     c                   return
010300081110     c                   else
010400081110     c                   except    testa
010500081110     c                   end
010600090109      * >>>>>>>
010700081209      * resto nel ciclo finchè....
010800081110     C                   DOU       SqlCod < 0
010900081110      * lettura cursore
011000081110     C/EXEC SQL
011100081110     C+ Fetch Next From A1 Into :FGT_REC, :dsFGT
011200081110     C/END-EXEC
011300081110      *  EoF
011400081110     C                   if        SqlCod = 100
011500081110     c                   Leave
011600081110      *  x errori
011700081110     C                   ElseIf    SqlCod < 0
011800081110     c                   eval      esito = Sqlcode
011900081110     c                   dump(a)
012000081110     c                   leave
012100081110     C                   ENDif
012200081110      *
012300090109     c                   clear                   eseguito          1
012400090109     c                   clear                   verifica          1
012500090102     c                   clear                   errore           10
012600081110      *
012700081110      * Solo se richiesto esecuzione aggiornamento altrimenti fa solo stampa
012800090109     c                   movel     stpSML        SMLPDR            8
012900090109     c                   move      stpPDR        Autista           7 0
013000090109     c                   move      Autista       SMLPDR
013100090109      *
013200090109      *  Solo in aggiornamento
013300081110     c                   if        Aggiorna = 'S'
013400081110      *
013500090109      *  Deve scrivere la testata
013600090109      *    a rottura di simulazione/autista
013700090109     c                   if        smlPDR <> savsmlPDR
013800090109      *     salva chiave di rottura
013900090109     c                   eval      savsmlPDR = smlPDR
014000090109     c                   clear                   fitgt000
014100090109     c                   z-add     STPPDR        tgtPDR
014200090401      *
014300090401      * rileva il peso dall'anagrafica
014400090401     c                   eval      apdTIP = 'A'
014500090401     c                   eval      apdPDR = tgtPDR
014600090401     c     Key_APD       chain     fiapd01l
014700090401     c                   if        %Found(fiapd01l)
014800090421     c                   movel     apdcsf        tgtsoc
014900090421     c                   movel     *all'0'       tgtcdf
015000090421     c                   move      apdksc        tgtcdf
015100090421      *---
015200090421     c                   movel     apdflr        dapdflr
015300090421     c                   eval      tgtpkl = §APDMCP
015400090421     c* reperisco i tempi di carico e scarico
015500090421     c                   clear                   ficn08ds
015600090421     c                   eval      i08data= tgtddt
015700090421     c                   eval      i08tip= 'A'
015800090421     c                   eval      i08mcp = tgtpkl
015900090421     c                   call      'FICN08R'
016000090421     c                   parm                    ficn08ds
016100090421     c                   if        o08err =  ' '
016200090421     c                   eval      TGTTMPC = o08tc
016300090421     c                   eval      TGTTMPS = o08ts
016400090421     c                   end
016500090401     c                   end
016600090401      *
016700090109     c                   move      STPSML        tgtSML
016800090109     c                   move      'EUR'         tgtDIV
016900090109     c                   z-add     200           tgtPRG
017000090420     c                   z-add     20090501      tgtDDT
017100090421      * data scadenza
017200090112     c                   z-add     20391231      tgtDST
017300090112      *
017400090109     c                   z-add     *date         tgtDUV
017500090109     c                   z-add     D§GPIGPI      tgtPCAR
017600090109     c                   moveL     'Conversion'  tgtPRU
017700090422      * Attenzione:
017800090422      *     se non sono state stampate non devono essere nemmeno convalidate
017900090422     c                   if        stpDTS > 0
018000090422     c                   z-add     20090501      tgtDTS
018100090422     c                   end
018200090422      *
018300090109     c                   write     fitgt000
018400090109     c                   end
018500081216      *
018600090109     c                   exsr      WRI_FPT_FPD
018700090109      *
018800090109      *------------------
018900081121      *  Solo se non ci sono stati errori:
019000081218      *   deve eliminare i puntamenti del codice di riferimento
019100090109     c                   if        eseguito = *blank
019200081222      *
019300081222     c                   open      fifgt00f
019400081218     c     fgt_REC       chain     FIFGT00F
019500081218     c                   if        %Found(FIFGT00F)
019600090112      *
019700090422      *  Fa scadere la testata al 30 apr.2009
019800090420     c                   if        fgtddt < 20090501
019900090422     c                   eval      fgtDST = 20090430
020000090112     c                   end
020100090112      *
020200090422      *     Ma se decorreva già dal 1/5/2009
020300090422      *   non può farlo scadere il 30/4/2009
020400090112      * e quindi questa diventa la tariffa della supertestata
020500090420     c                   if        fgtddt = 20090501
020600090112     c                   z-add     200           fgtPRG
020700090112     c                   z-add     *date         fgtDUV
020800090112     c                   clear                   fgtDCN
020900090112     c                   clear                   fgtDTS
021000090112     c                   clear                   fgtFTR
021100090112     c                   clear                   fgtDTR
021200090112     c                   end
021300090112      *
021400081218     c                   update    fgtFisico
021500081121     c                   endif
021600090109      *
021700090109      *  Quindi scrive un'altra testata con decorrenza 2 feb.2009
021800090112      *   se non nel caso di coincidenza della decorrenza 1/2/2009
021900090420     c                   if        fgtddt < 20090501
022000090112      *
022100090420     c                   eval      fgtDDT = 20090501
022200090109     c                   eval      fgtDST = 20391231
022300090109     c                   z-add     200           fgtPRG
022400090109     c                   z-add     *date         fgtDUV
022500090109     c                   clear                   fgtDCN
022600090109     c                   clear                   fgtDTS
022700090109     c                   clear                   fgtFTR
022800090109     c                   clear                   fgtDTR
022900090109     c                   write     fgtFisico
023000090112      *
023100090112     c                   end
023200090112      *
023300081222     c                   close     fifgt00f
023400090109      *
023500090109     c                   end
023600090109     c                   endif
023700090109      *------------------
023800081230     c                   if        eseguito = '1'
023900081230     c                   movel(p)  'Errore'      errore
024000081210     c                   endIf
024100090102      *
024200081210      *  se sono qui ho letto un record correttamente
024300081210     c   Of              except    testa
024400081210     c                   except    detta
024500081110      *
024600081110     C                   ENDDO
024700081110     C/EXEC SQL
024800081110     C+ Close A1
024900081110     C/END-EXEC
025000081110     C                   if        Esito < 0
025100081110     c                   return
025200081110     c                   endIF
025300081110     C*
025400081110     C     FINE          TAG
025500081110     c                   except    finest
025600081110     c                   return
025700081209     C*------------------------------------------------------------------------*
025800081209     C* *INZSR - OPERAZIONI INIZIALI
025900081209     C*------------------------------------------------------------------------*
026000081209     C     *INZSR        BEGSR
026100081209      *
026200081209     C     *ENTRY        PLIST
026300081209     C                   PARM                    AGGIORNA          1
026400081209     C*
026500090109      * duplica le tariffe FGT
026600081209     C     fgt01K        kLIST
026700081211     C                   kfld                    stpPDR
026800081211     C                   kfld                    stpSML
026900081211     C                   kfld                    stpTSR
027000081211     C                   kfld                    stpCTR
027100081211     C                   kfld                    stpPRG
027200090109      *
027300090109      * duplica le tariffe FPT e FPD
027400090109     c     Key_FPT       KLIST
027500090109     c                   kfld                    stpPDR
027600090109     c                   kfld                    stpSML
027700090109     c                   kfld                    stpTSR
027800090109     c                   kfld                    stpCTR
027900090109     c                   kfld                    stpPRG
028000081209     C*
028100090401     c     Key_APD       KLIST
028200090401     c                   kfld                    apdTIP
028300090401     c                   kfld                    apdPDR
028400090401     C*
028500081209     c                   clear                   esito
028600081209      *
028700081209     C* COSTRUISCO LA DATA FINALE IN FORMATO AAAAMMGG
028800081209     C                   clear                   UDATE_8           8 0
028900081209     C                   MOVEL     *DATE         UDATE_8
029000090109      *
029100090109      * reperisce il prezzo di riferimento del carburante
029200090109     c                   call      'FICN05R'
029300090109     c                   parm                    ficn05DS
029400090109      *
029500090109     c                   iF        o05err = *blank
029600090109     c                   eval      dGPI = o05GPI
029700090109     c                   enD
029800090109      *
029900081209     c                   endSR
030000090109      *---------------------------------------------------------------*
030100090109      *  Scrive Testate e Dettaglio
030200090109      *---------------------------------------------------------------*
030300090109     c     wri_FPT_FPD   begSR
030400090109     C*
030500090109     c     Key_FPT       setll     fifpt01L
030600090109     c     Key_FPT       reade     fifpt01L
030700090109     c                   Dow       not %EoF(fifpt01l)
030800090109     c                   z-add     200           fptPRG
030900090109     c                   write     fifpt000
031000090109     c     Key_FPT       reade     fifpt01L
031100090109     c                   endDo
031200090109     C*
031300090109     c     Key_FPT       setll     fifpd01L
031400090109     c     Key_FPT       reade     fifpd01L
031500090109     c                   Dow       not %EoF(fifpd01l)
031600090109     c                   z-add     200           fpdPRG
031700090109     c                   write     fifpd000
031800090109     c     Key_FPT       reade     fifpd01L
031900090109     c                   endDo
032000090109     C*
032100090109     c                   endSR
032200081211     C*---------------------------------------------------------------*
032300081110     OQSYSPRT   E            TESTA            02
032400081110     O                                           16 'Simul'
032500081110     O                                           25 'Autista'
032600081110     O                                           30 'TSR'
032700081110     O                                           38 'Tariffa'
032800081110     O                                           45 'Progr'
032900081110     O                                           55 'Data_Dec'
033000081110     O                                           65 'Data_Sca'
033100081110     O                                           75 'Data_Stp'
033200081110     O                                           85 'Data_Cnv'
033300081211     O                                          115 'N_Rec_FGT'
033400081110     O          E            DETta       1
033500081110     O                       stpSML              14
033600081110     O                       stpPDR        z     25
033700081110     O                       stpTSR              29
033800081110     O                       stpCTR              36
033900081110     O                       stpPRG        z     43
034000081110     O                       stpDDT        z     55
034100081110     O                       stpDST        z     65
034200081110     O                       stpDTS        z     75
034300081110     O                       stpDCN        z     85
034400081211     o                       FGT_REC       z    115
034500081230     O                       errore             130
034600081110      ****
034700081110     o          E            FINEST      2
034800090109     o                                           30 '*** TNVRA15R ***'
034900090102     o                                           59 '***   FINE  STAMPA  ***'
035000090109     o                                          + 9 '***  SUPER TARIFFA  ***'
