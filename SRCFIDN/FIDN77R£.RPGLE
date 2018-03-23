000100120423     /*PRM  dbgview(*source)
000200120423     /*END
000300020424      ****************************************************************
000400110729      *    Elenco C.A. per rivalsa EUROEXPRESS  -   batch            *
000500020424      ****************************************************************
000600020424
000700020424      ****************************************************************
000800020424      *  RIEPILOGO INDICATORI                                        *
000900020424      *--------------------------------------------------------------*
001000020424      * 01 - RISTAMPA                                                *
001100020424      * 02 - STAMPO Avviso Danno                                     *
001200020424      * 03 - STAMPO Evento                                           *
001300020620      * 04 - STAMPO Causale Chiusura                                 *
001400110729      * 41 - EOF FIDN77P                                             *
001500020424      ****************************************************************
001600020424
001700020424     H DECEDIT('0,') DATEDIT(*DMY.)
001800020424
001900110729     FFNDCT13L  IF   E           K DISK
002000110729     FFNDCT01L  UF   E           K DISK    rename(fndct000:fndct01)
002100020424     FTITA430C  IF   E           K DISK
002200020424     FTITAS30C  IF   E           K DISK
002300020424     FTNTMD01L  IF   E           K DISK
002400020424     FFNDCS01L  IF   E           K DISK
002500020424     FFNDET01L  IF   E           K DISK
002600020424     FFNDCL01L  IF   E           K DISK
002700110729      *
002800110801     FWFDEX00F  UF a E           K DISK
002900020424      *
003000110729     FFIDN77P   O    E             PRINTER oflind(*IN41) usropn
003100020424
003200020424      *------------- SCHIERE
003300020424      * - Note dal file FNDCS00F
003400020424     D $NoteA          s                   like(DCSnot) dim(3) inz
003500020424      *
003600020424      *------------- DS ESTERNE
003700020424      * - Parametri
003800020424     D KPJBA         e ds
003900110729     D FIDN76DS      e ds                  inz
004000050711      *
004100050711      * - Parametri x Controllo profilo utenti
004200050711     d TIBS34ds      e ds
004300050711      * - Ds di riferimento al file esterno AZUTE00F
004400050711     d AZUTEds       e ds                  extname(AZUTE00F)
004500050711      * - Ds per dati organigramma
004600050711     d dDATIUTE      e ds
004700020424      *
004800060613     D DSBL4I        e ds                  inz
004900020424      *
005000020424      * Flag operativi - campo DCTFLO del file FNDCT00F
005100020424     D DDCT01        e ds                  inz
005200110729      *
005300110729      * Partner EuroExpress
005400110729     D FIDN40DS      e ds                  inz
005500120423      *
005600110729      * Reperimento dati anagrafici
005700110729     d TIBS69ds      e ds
005800110729     d DS_cnaco      e ds                  inz extname(CNACO00F)
005900110729     d DS_cnind      e ds                  inz extname(CNIND00F)
006000110729     d DS_cnclp      e ds                  inz extname(CNCLP00F)
006100110729     d DS_fncls      e ds                  inz extname(FNCLS00F)
006200020424      *
006300020424      * Tabelle
006400020424     D TIBS02DS      e ds                  inz
006500020424     D DTADdct       e ds                  extname(DTAD)     inz
006600020424     D                                     prefix(A)
006700020424     D DTADdet       e ds                  extname(DTAD)     inz
006800020424     D                                     prefix(E)
006900020424     D DCCH          e ds                  inz
007000160728     d dGEDdn        e ds                  qualified         inz
007100160728     d dSTD          e ds                  inz
007200020424      *
007300020424      * Ridefinizione tracciati record
007400020424     D DS_TITAS      e ds                  extname(TITAS30C) inz
007500020424     D DS_TNTMD      e ds                  extname(TNTMD00F) inz
007600020424     D DS_FNDCS      e ds                  extname(FNDCS00F) inz
007700020424     D DS_FNDCL      e ds                  extname(FNDCL00F) inz
007800020424      *
007900020424      *------------- DS DI STATO
008000020424     D STATUS         sds
008100020424     D  VTCPGM           *proc
008200020424      *
008300020424      *------------- DS INTERNE
008400020424      * - Controllo/Inversione date
008500020424     D WLBDAT          ds                  inz
008600020424     D  G02DAT                 1      8  0
008700020424     D  G02INV                 9     16  0
008800020424     D  G02ERR                17     17
008900020424     D  G02TGI                18     22  0
009000020424      * - Chiave tipo descrizione per file FNDCS01L
009100020424     D DSnumCA         ds                  inz
009200020424     D  DCTaac
009300020424     D  DCTfil
009400020424     D  DCTnca
009500020424      *
009600020424      *------------- VARIABILI
009700020424      * - data in formato AAAAMMGG
009800020424     D WdataYMD        s              8  0 inz
009900020424      * - data in formato GIULIANO
010000020424     D WdataJUL        s               d   inz datfmt(*JUL)
010100020424      * - data odierna in formato GIULIANO
010200020424     D WoggiJUL        s               d   inz datfmt(*JUL)
010300020424      * - campo di comodo per memorizzazione DCTPER
010400020424     D WdctPER         s                   inz like(DCTper)
010500110729      * - campi di comodo per salto pagina cambio  PARTNER
010600110729     D SavCod          s                       like(DEXcod)
010700120423      *
010800020424      * - indice schiera
010900020424     D XX              s              3  0 inz
011000120423      *
011100020424      * - flag record da elaborare
011200020424     D $OKrec          s              1    inz(*off)
011300020424      * - flag stampato almeno un record
011400020424     D $Stampa         s              1    inz(*off)
011500020424      *
011600020424      *------------- COSTANTI
011700020619      * - causale di chiusura C.A.
011800020619     D Wchca           c                   const(900)
011900020424
012000020424      ****************************************************************
012100020424      *                   C   I   C   L   O                          *
012200020424      ****************************************************************
012300020424
012400020424     c     *ENTRY        PLIST
012500020424     c                   PARM                    KPJBA
012600020424      *
012700020424      * operazioni iniziali
012800020424     c                   exsr      SR_INZSR
012900020424      *
013000020424      * mi posiziono con la data sul file testate C.A.
013100110801     c                   if        I76ris   = 'S'
013200020424     c                   eval      *in01    = *on
013300020424     c                   move      WdataJUL      WdctPER
013400020424     c                   else
013500020424     c                   eval      *in01    = *off
013600020424     c                   eval      WdctPER  = *all'9'
013700020424     c                   endif
013800020424     c     WdctPER       setll     FNDCT000
013900020424      *
014000020424      * ciclo di lettura del file principale
014100020424      *
014200020424do  1c                   do        *hival
014300020424      *
014400110801     c     WdctPER       reade     FNDCT000
014500020424      *
014600020424      * e.o.f.
014700020424if  2c                   if        %eof(FNDCT13L)
014800020424     c                   leave
014900020424e   2c                   endif
015000020424      *
015100020424      * selezione record
015200020424     c                   exsr      SR_SELEZ
015300020424if  2c                   if        $OKrec = *off
015400020424     c                   iter
015500020424e   2c                   endif
015600020424      *
015700020424      * recupero dei dati aggiuntivi per la stampa
015800020424     c                   exsr      SR_DATI
015900110729      *
016000110729      * scrittura
016100110729     c                   exsr      SR_WRITE
016200020424      *
016300020424e   1c                   enddo
016400110729     c                   feod      WFDEX00F
016500110729      *
016600110729      * stampa
016700120423     c                   if        I76ele <> 'F'
016800110729     c                   exsr      SR_STAMPA
016900120423     c                   endif
017000020424      *
017100020424      * Fine
017200020424     c                   exsr      SR_ENDPGM
017300020424
017400020424      ****************************************************************
017500020424      * ROUTINE INIZIALE
017600020424      ****************************************************************
017700020424     c     SR_INZSR      BEGSR
017800020424      *
017900110729     c                   movel     KPJBU         FIDN76DS
018000020424      *
018100110729      * Reperisco nome azienda
018200050711     c     *dtaara       define    §azute        AZUTEds
018300050711     c     *dtaara       define    §datiute      dDATIUTE
018400050711     c                   in(E)     *dtaara
018500050711     c                   if        %ERROR or RSUT = *blanks
018600050711     c                   clear                   Tibs34Ds
018700050711     c                   call      'TIBS34R'
018800050711     c                   parm                    Tibs34Ds
018900050711     c                   in        *dtaara
019000050711     c                   endif
019100020424      *
019200020424     c                   move      *date         WoggiJUL
019300020424      *
019400050711     c                   eval      *in41    = *on
019500020424      *
019600110801if  1c                   if        I76ris   = 'S'
019700110801     c                   eval      WdataYMD = I76dat
019800020424     c                   exsr      CnvDat
019900020424     c                   z-add     G02dat        PTdata
020000020424x   1c                   else
020100020424     c                   move      *date         PTdata
020200020424e   1c                   endif
020300160728      *
020400160728      * recupero la moneta corrente
020500160728     c                   clear                   TIBS02ds
020600160728     c                   eval      T02mod = 'C'
020700160728     c                   eval      T02sif = KNSIF
020800160728     c                   eval      T02cod = 'GED'
020900160728     c                   eval      T02ke1 = 'DANNI'
021000160728     c                   call      'TIBS02R'
021100160728     c                   parm                    KPJBA
021200160728     c                   parm                    TIBS02ds
021300160728     c                   movel     T02uni        dGEDdn
021400160728      *
021500160728      * Reperimento tabella Standards Danni
021600160728     c                   clear                   TIBS02ds
021700160728     c                   eval      T02mod = 'C'
021800160728     c                   eval      T02sif = KNSIF
021900160728     c                   eval      T02cod = 'STD'
022000160728     c                   eval      T02ke1 = '1 '
022100160728     c                   eval      T02ke2 = dGEDDN.§GEDdba
022200160728     c                   call      'TIBS02R'
022300160728     c                   parm                    KPJBA
022400160728     c                   parm                    TIBS02ds
022500160728     c                   if        T02err = *blanks
022600160728     c                   movel     T02uni        dSTD
022700160728     c                   endif
022800020424      *
022900020424     c                   ENDSR
023000020424
023100020424      ****************************************************************
023200020424      * SELEZIONE DEI RECORD DA ELABORARE
023300020424      ****************************************************************
023400020424     c     SR_SELEZ      BEGSR
023500020424      *
023600020424     c                   reset                   $OKrec
023700021120      *
023800021120      * Leggo una v.l. che prevede già:
023900021120      * - la OMIT dei record con il campo DCTper = *zeros
024000021120      * Tale campo è impostato dagli appositi programmi SOLO per:
024100021120      * - spedizioni Porto Franco, export DPD
024200021120      *              (§DCTport = 'F', §DCTtisp = 'E' e §DCTeuro = 'D')
024300021120      * - spedizioni export FEDEX    (§DCTtisp = 'E' e §DCTeuro = 'F')
024400110729      * - spedizioni export EUROEXPRES (DCTtisp = 'E' e §DCTeuro = 'X') e partner con rivalsa
024500021120      * - comunque con responsabilità PARTNER           (DCTres = 'P')
024600021120      * - e con tipo gestione "Pratica Assicurativa"    (DCTfpr = 'P')
024700020424      *
024800020424     c                   movel     DCTflo        dDCT01
024900020424      *
025000020424      * record annullati
025100020424     c     DCTatb        cabne     *blanks       EndSelez
025200110729      * Network estero non EUROEXPRESS
025300110729     c     §DCTeuro      cabne     'X'           EndSelez
025400120413      *
025500120413      * Aggancio file liquidazione C.A.
025600120413     c     K03DCL01      chain     FNDCL000
025700120413if  1c                   if        not %found(FNDCL01L)
025800120413     c                             or  DCLatb <> *blanks
025900120413     c                   clear                   DS_FNDCL
026000120413e   1c                   endif
026100120413      * importo da liquidare maggiore o uguale a 100
026200160728      * (ora tale importo è contenuto in tab. "STD": campo §STDIPDEEX)
026300160728     c*//  dclipl        cablt     100           EndSelez
026400160728     c     dclipl        cablt     §STDipdEEX    EndSelez
026500020619      * Fase 900 (se NON RIstampa)
026600110801if  1c                   if            I76ris <> 'S'
026700020619     c                             and DCTfca <> WCHCA
026800020619     c                   goto      EndSelez
026900020619e   1c                   endif
027000020424      *
027100020424      * record da elaborare
027200020424     c                   eval      $OKrec = *on
027300020424      *
027400020424     c     EndSelez      ENDSR
027500020424
027600020424      ****************************************************************
027700020424      * RECUPERO DEI DATI AGGIUNTIVI PER LA STAMPA
027800020424      ****************************************************************
027900020424     c     SR_DATI       BEGSR
028000020424      *
028100020424      * Aggancio file bolle sede
028200020424     c     K04DCT02      chain     TITAS30C
028300020424if  1c                   if        not %found(TITAS30C)
028400020424     c                   clear                   DS_TITAS
028500020424e   1c                   endif
028600020424      *
028700020424      * Aggancio file riferimenti bolle
028800060613     c                   eval      TA4trc = 'I'
028900020424     c     K05TA430      chain     TITA430C
029000020424if  1c                   if        %found(TITA430C)
029100060613     c                   movel     TA4not        DSBL4I
029200020424x   1c                   else
029300060613     c                   clear                   DSBL4I
029400020424e   1c                   endif
029500020424      *
029600020424      * Aggancio file mittenti/destinatari
029700020424     c     K04DCT02      chain     TNTMD000
029800020424if  1c                   if        not %found(TNTMD01L)
029900020424     c                   clear                   DS_TNTMD
030000020424e   1c                   endif
030100020424      *
030200020424      * Aggancio testata evento danni
030300020424if  1c                   if        DCTnev > *zeros
030400020424     c                   eval      *in03  = *on
030500020424     c     K02DET01      chain     FNDET000
030600020424if  2c                   if        not %found(FNDET01L)
030700020424     c                             or  DETatb <> *blanks
030800020424     c                   clear                   DETtad
030900020424e   2c                   endif
031000020424x   1c                   else
031100020424     c                   eval      *in03  = *off
031200020424     c                   clear                   DETtad
031300020424e   1c                   endif
031400020424      *
031500020424      * Aggancio file descrizioni per descrizione anomalia
031600020424     c                   clear                   XX
031700020424     c                   clear                   $NoteA
031800020424     c                   clear                   DS_FNDCS
031900020424     c                   movel     'C'           DCStpd
032000020424     c                   movel(p)  DSnumCA       DCSnkt
032100020424     c                   movel     'A'           DCSstd
032200020424     c                   clear                   DCSdim
032300020424     c                   clear                   DCShim
032400020424     c                   clear                   DCSnks
032500020424     c                   movel     'D'           DCStrc
032600020424     c     K07DCS01      chain     FNDCS000
032700020424if  1c                   if        %found(FNDCS01L)
032800020424do  2c                   dou           %eof(FNDCS01L)
032900020424     c                             or  XX >= 3
033000020424if  3c                   if        DCSatb =  *blanks
033100020424     c                   add       1             XX
033200020424     c                   movel     DCSnot        $NoteA(XX)
033300020424e   3c                   endif
033400020424     c     K07DCS01      reade     FNDCS000
033500020424e   2c                   enddo
033600020424e   1c                   endif
033700020424      *
033800020424      * Aggancio file descrizioni per descrizione pezzi mancanti
033900020424     c                   clear                   DS_FNDCS
034000020424if  1c                   if        DCTnpz > *zeros
034100020424     c                   movel     'C'           DCStpd
034200020424     c                   movel(p)  DSnumCA       DCSnkt
034300020424     c                   movel     'P'           DCSstd
034400020424     c                   clear                   DCSdim
034500020424     c                   clear                   DCShim
034600020424     c                   clear                   DCSnks
034700020424     c                   movel     'D'           DCStrc
034800020424     c     K07DCS01      chain     FNDCS000
034900020424do  1c                   if        not %found(FNDCS01L)
035000020424     c                             or  DCSatb <> *blanks
035100020424     c                   clear                   DCSnot
035200020424e   1c                   endif
035300020424e   1c                   endif
035400020424      *
035500020424      * Decodifico tipo anomalia
035600020424     c                   clear                   TIBS02DS
035700020424     c                   movel     'TAD'         T02cod
035800020424     c                   movel(p)  DCTtad        T02ke1
035900020424     c                   exsr      CallTIBS02
036000020424     c                   movel     T02uni        DTADdct
036100020424      *
036200020424      * Decodifico evento
036300020424     c                   clear                   DTADdet
036400020424if  1c                   if        *in03
036500020424     c                   clear                   TIBS02DS
036600020424     c                   movel     'TAD'         T02cod
036700020424     c                   movel     DETtad        T02ke1
036800020424     c                   exsr      CallTIBS02
036900020424     c                   movel     T02uni        DTADdet
037000020424e   1c                   endif
037100020424      *
037200020424      * Decodifico eventuale causale di chiusura
037300020424     c                   clear                   DCCH
037400020620     c                   eval      *in04      =  *off
037500020620if  1c                   if        DCTcch   <> *blanks
037600020620     c                   eval      *in04      =  *on
037700020424     c                   clear                   TIBS02DS
037800020424     c                   movel     'CCH'         T02cod
037900020424     c                   movel(p)  DCTcch        T02ke1
038000020424     c                   exsr      CallTIBS02
038100020424     c                   movel     T02uni        DCCH
038200020424e   1c                   endif
038300020424      *
038400020424     c                   ENDSR
038500110729      *
038600110729      ****************************************************************
038700110729      * SCRIVO IL RECORD DI LAVORO
038800110729      ****************************************************************
038900110729     c     SR_WRITE      BEGSR
039000110729      *
039100110729      * IMPOSTAZIONE CAMPI DEL FILE
039200110729     c                   clear                   WFDEX000
039300120423      *
039400120423      * - Estrazione
039500120423     c                   eval      DEXute     =  KNmUs
039600120423     c                   eval      DEXdtec    =  *date
039700110729      *
039800110729      * - Avviso Presunto Danno
039900110729     c                   eval      DEXdit     =  DCTdit
040000110729     c                   eval      DEXPrn     =  DCTprn
040100110729     c                   eval      DEXpra     =  DCTpra
040200110729      * - Comunicazione Anomalia
040300110729     c                   eval      DEXfil     =  DCTfil
040400110729     c                   eval      DEXnca     =  DCTnca
040500110729     c                   eval      DEXaac     =  DCTaac
040600110729      * - Spedizione
040700110729     c                   eval      DEXlnp     =  DCTlnp
040800110729     c                   eval      DEXnrs     =  DCTnrs
040900110729     c                   eval      DEXnsp     =  DCTnsp
041000110729     c                   z-add     TASmgs        WdataYMD
041100110729     c                   movel     DCTaas        WdataYMD
041200110729     c                   exsr      CNVDAT
041300110729     c                   z-add     G02dat        DEXdsp
041400110729      * - Partner
041500110729     c                   clear                   fidn40ds
041600110801     c                   eval      I40dsp = ((TASaas*10000)+TASmgs)
041700110729     c                   If        §dcttisp = 'E'
041800110729     c                   eval      I40fil = dctlna
041900110729     c                   endif
042000110729     c                   call      'FIDN40R'
042100110729     c                   parm                    fidn40ds
042200110729     c                   If        o40cod <> *zeros
042300110729     c                   eval      DEXcod = O40cod
042400110801     c                   else
042500110801     c                   leavesr
042600110729     c                   endif
042700120502     c                   If        SavCod  = 0   or
042800120423     c                             Savcod <> DEXcod
042900120502     c                   eval      Savcod  = DEXcod
043000120423     c                   clear                   TIBS69DS
043100120423     c                   z-add     DEXcod        I69kac
043200120423     c                   CALL      'TIBS69R'
043300120423     c                   PARM                    tibs69DS
043400120423     c                   PARM                    DS_cnaco
043500120423     c                   PARM                    DS_cnind
043600120423     c                   PARM                    DS_cnclp
043700120423     c                   PARM                    DS_fncls
043800120502     c                   EndIf
043900120423     c                   If        o69err = ' '
044000120423     c                   movel     ACOrag        DEXrag
044100120502     c                   Else
044200120423     c                   eval      DEXrag = *all'?'
044300120502     c                   EndIf
044400110729      * - Mittente
044500110729     c                   movel     TMDrsm        DEXrsm
044600110729     c                   movel     TMDinm        DEXinm
044700110729     c                   movel     TMDcam        DEXcam
044800110729     c                   movel     TMDlom        DEXlom
044900110729     c                   movel     TMDprm        DEXprm
045000110801     c                   movel     TMDnzm        DEXnam
045100110729      * - Destinatario
045200110729     c                   movel     TMDrsd        DEXrsd
045300110729     c                   movel     TMDind        DEXind
045400110729     c                   movel     TMDcad        DEXcad
045500110729     c                   movel     TMDlod        DEXlod
045600110729     c                   movel     TMDprd        DEXprd
045700110801     c                   movel     TMDnzd        DEXnad
045800110729      *
045900110729      * - Colli in bolla
046000110729     c                   eval      DEXncl     =  TASncl
046100110729      * - Peso Kg in bolla
046200110729     c                   eval      DEXpkf     =  TASpkf
046300110729      * - Evento
046400110729if  1c                   if        *in03
046500110729     c                   eval      DEXaae = DCTaae
046600110729     c                   eval      DEXnev = DCTnev
046700110729if  2c                   if        E§TADdest  <> *blanks
046800110729     c                   movel     E§TADdest     DEXtae
046900110729x   2c                   else
047000110729     c                   movel     E§TADdesc     DEXtae
047100110729e   2c                   endif
047200110729e   1c                   endif
047300110729      *
047400110729      * Campi del rec. DN77PD3:
047500110729      * - Colli danneggiati/mancanti
047600110729     c                   eval      DEXncn     =  DCTncn
047700110729      * - Peso Kg danneggiato/mancanti
047800110729     c                   eval      DEXpkd     =  DCTpkd
047900110729      * - Pezzi danneggiati/mancanti
048000110729     c                   eval      DEXnpz     =  DCTnpz
048100110729      * - Note
048200110729     c                   movel     DCSnot        DEXdpz
048300110729      *
048400110729      * Campi del rec. DN77PD4:
048500110729      * - Anomalia
048600110729if  1c                   if        A§TADdest  <> *blanks
048700110729     c                   movel     A§TADdest     DEXtad
048800110729x   1c                   else
048900110729     c                   movel     A§TADdesc     DEXtad
049000110729e   1c                   endif
049100110729      * - Note aggiuntive (descrizione anomalia)
049200110729     c                   movel     $NoteA(1)     DEXdan
049300110729     c                   movel(p)  $NoteA(2)     DEXkos
049400110729     c                   move      $NoteA(3)     DEXkos
049500110729      *
049600110729      * - Importo rimborsato
049700110729if  1c                   if        DCLipl < DCLipt
049800110729     c                   eval      DEXipr = 'VEDI C.A.'
049900110729x   1c                   else
050000110729     c                   eval      DEXipr = %trim(%editc(DCLipl:'K'))
050100110729e   1c                   endif
050200110729      * - Causale di chiusura
050300110729if  1c                   if        DCTcch     <> *blanks
050400110729     c                   eval      *in04      =  *on
050500110729     c                   eval      DEXcch     =  DCTcch
050600110729     c                   eval      DEXccd     =  §CCHdesc
050700110729x   1c                   else
050800110729     c                   clear                   DEXcch
050900110729     c                   clear                   DEXccd
051000110729e   1c                   endif
051100110729      *
051200110729      * SCRITTURA DLE RECORD
051300110729     C                   WRITE     WFDEX000
051400110729      *
051500110729     c                   ENDSR
051600020424
051700020424      ****************************************************************
051800110729      * STAMPA IL RECORD DI LAVORO
051900020424      ****************************************************************
052000110729     c     SR_STAMPA     BEGSR
052100020424      *
052200110729      * Lettura del file di lavoro
052300120423     c                   clear                   Savcod
052400110729     c     *loval        setll     WFdex00f
052500110729     c                   do        *hival
052600110729     c                   read      WFdex00f
052700110729     c                   If        %eof(WFdex00f)
052800110729     c                   leave
052900110729     c                   endif
053000110729      * verifico se cambiato partner
053100110729     c                   If        SavCod = 0 or
053200110729     c                             Savcod <> DEXcod
053300110729     c                   eval      Savcod = DEXcod
053400110801     c                   eval      PTDcod = DEXcod
053500120423     c                   eval      PTDdes = DEXrag
053600110729     c                   eval      *in41 = *on
053700110801     c                   endif
053800020424      *
053900110729      * Campi del rec. DN77PD1:
054000020424      * - Avviso Presunto Danno
054100110729     c                   if           DEXdit  <> *blanks
054200110729     c                             or DEXprn  <> *zeros
054300110729     c                             or DEXpra  <> *zeros
054400020424     c                   eval      *in02      =  *on
054500110729     c                   eval      PD1dit     =  DEXdit
054600110729     c                   eval      PD1prn     =  DEXprn
054700110729     c                   eval      PD1pra     =  DEXpra
054800020424     c                   else
054900020424     c                   eval      *in02      =  *off
055000020424     c                   endif
055100020424      * - Comunicazione Anomalia
055200110729     c                   eval      PD1fil     =  DEXfil
055300110729     c                   eval      PD1nca     =  DEXnca
055400110729     c                   eval      PD1aac     =  DEXaac
055500020424      * - Spedizione
055600110729     c                   eval      PD1lnp     =  DEXlnp
055700110729     c                   eval      PD1nrs     =  DEXnrs
055800110729     c                   eval      PD1nsp     =  DEXnsp
055900110729     c                   z-add     DEXDSP        PD1dsp
056000020424      * - Mittente
056100110729     c                   movel     DEXrsm        PD1rsm
056200110729     c                   movel     DEXinm        PD1inm
056300110729     c                   movel     DEXcam        PD1cam
056400110729     c                   movel     DEXlom        PD1lom
056500110729     c                   movel     DEXprm        PD1prm
056600110801     c                   movel     DEXnam        PD1nzm
056700020424      * - Destinatario
056800110729     c                   movel     DEXrsd        PD1rsd
056900110729     c                   movel     DEXind        PD1ind
057000110729     c                   movel     DEXcad        PD1cad
057100110729     c                   movel     DEXlod        PD1lod
057200110729     c                   movel     DEXprd        PD1prd
057300110801     c                   movel     DEXnad        PD1nzd
057400020424      *
057500110729      * Campi del rec. DN77PD2:
057600020424      * - Colli in bolla
057700110729     c                   eval      PD2ncl     =  DEXncl
057800020424      * - Peso Kg in bolla
057900110729     c                   eval      PD2pkf     =  DEXpkf
058000020424      * - Evento
058100110729     c                   if        dexnev > 0
058200110729if  1c                   eval      *in03 = *on
058300110729     c                   eval      PD2kev     =  %editc(DEXaae:'Z') + '/' +
058400110729     c                                        %trim(%editc(DEXnev:'Z'))
058500110729     c                   movel     DEXtae        PD2tae
058600110729     c                   else
058700110729if  1c                   eval      *in03 = *off
058800020424e   1c                   endif
058900020424      *
059000110729      * Campi del rec. DN77PD3:
059100020424      * - Colli danneggiati/mancanti
059200110729     c                   eval      PD3ncn     =  DEXncn
059300020424      * - Peso Kg danneggiato/mancanti
059400110729     c                   eval      PD3pkd     =  DEXpkd
059500020424      * - Pezzi danneggiati/mancanti
059600110729     c                   eval      PD3npz     =  DEXnpz
059700020424      * - Note
059800110801     c                   movel     DCSnot        DEXdpz
059900020424      *
060000110729      * Campi del rec. DN77PD4:
060100020424      * - Anomalia
060200110729     c                   movel     DEXtad        PD4tad
060300020424      * - Note aggiuntive (descrizione anomalia)
060400110729     c                   movel     DEXdan        PD4dan
060500110729     c                   movel     DEXkos        PD4kos
060600020424      *
060700110729      * Campi del rec. DN77PD5:
060800020620      * - Importo rimborsato
060900110729     c                   eval      PD5ipr = DEXipr
061000020424      * - Causale di chiusura
061100110729if  1c                   if        DEXcch     <> *blanks
061200020620     c                   eval      *in04      =  *on
061300110729     c                   eval      PD5cch     =  DEXcch
061400110729     c                   eval      PD5ccd     =  DEXccd
061500020424x   1c                   else
061600020620     c                   clear                   PD5cch
061700020620     c                   clear                   PD5ccd
061800020424e   1c                   endif
061900020424      *
062000020424      * STAMPA
062100020620      *
062200020424      * - apertura iniziale del prtf
062300020424if  2c                   if        $Stampa    =  *off
062400110729     c                   open      FIDN77P
062500020424e   2c                   endif
062600120416      * - separatore
062700120416     c                   if        $Stampa    =  *on
062800120416     c                   write     DN77P1
062900120416     c                   endif
063000020424      * - testata
063100050711     c                   if        *IN41
063200110729     c                   write     DN77PT
063300020424     c                   eval      *IN41      =  *off
063400020424     c                   endif
063500020424      * - dettaglio
063600110729     c                   write     DN77PD1
063700110729     c                   write     DN77PD2
063800110729     c                   write     DN77PD3
063900110729     c                   write     DN77PD4
064000110729     c                   write     DN77PD5
064100020424     c                   eval      $Stampa    =  *on
064200020620      *
064300020424      *
064400020424      * AGGIORNAMENTO DEL RECORD IN ESAME DEL FILE FNDCT13L
064500050711      *   (se NON ristampa)
064600020424      *
064700110801     c                   if        I76ris     =  *blanks
064800110729     c     K03DCT01      chain     fndct01l
064900110729     c                   If        %found(fndct01l)
065000020424     c                   move      WoggiJUL      DCTper
065100110729     c                   update    FNDCT01
065200110729     c                   endif
065300020424     c                   endif
065400020424      *
065500110801     c                   enddo
065600110801      *
065700020424     c                   ENDSR
065800020424
065900020424      ****************************************************************
066000020424      * ROUTINE FINALE
066100020424      ****************************************************************
066200020424     c     SR_ENDPGM     BEGSR
066300020424      *
066400020424      * Segnalo la mancanza di dati da stampare
066500120424if  1c                   if             $Stampa  = *off
066600120424     c                             and  I76ele  <> 'F'
066700110729     c                   open      FIDN77P
066800110729     c                   write     DN77PT
066900110729     c                   write     DN77PD0
067000020424     c                   eval      $Stampa  = *on
067100020424e   1c                   endif
067200020424      *
067300020424      * Stampo "Fine Stampa"
067400020424if  1c                   if        $Stampa  = *on
067500110729     c                   write     DN77PE
067600020424e   1c                   endif
067700020424      *
067800020424      * Chiudo TIBS02R
067900020424     c                   clear                   TIBS02DS
068000020424     c                   movel     'C'           T02tla
068100020424     c                   call      'TIBS02R'
068200020424     c                   parm                    KPJBA
068300020424     c                   parm                    TIBS02DS
068400020424      *
068500020424      * Chiudo archivi aperti
068600020424     c                   close     FNDCT13L
068700020424     c                   if        $Stampa  = *on
068800110729     c                   close     FIDN77P
068900020424     c                   eval      $Stampa  = *off
069000020424     c                   endif
069100020424      *
069200020424      * Termino !
069300020424     c                   eval      *INLR    = *on
069400020424     c                   return
069500020424      *
069600020424     c                   ENDSR
069700020424
069800020424      ****************************************************************
069900020424      * RICHIAMO PGM RICERCA/CONTROLLO TABELLE
070000020424      ****************************************************************
070100020424     c     CallTIBS02    BEGSR
070200020424      *
070300020424     c                   movel     'C'           T02mod
070400020424     c                   movel     KNSIF         T02sif
070500020424      *
070600020424     c                   call      'TIBS02R'
070700020424     c                   parm                    KPJBA
070800020424     c                   parm                    TIBS02DS
070900020424      *
071000020424     c                   ENDSR
071100020424
071200020424      ****************************************************************
071300020424      * CONVERSIONE DATA DAL FORMATO AAAA/MM/GG AL FORMATO AA/GGG
071400020424      ****************************************************************
071500020424     c     CNVDAT        BEGSR
071600020424      *
071700020424     c                   clear                   WLBdat
071800020424     c                   eval      G02inv = WdataYMD
071900020424     c                   eval      G02err = '3'
072000020424     c                   call      'XSRDA8'
072100020424     c                   parm                    WLBDAT
072200020424      *
072300110801     c                   If        g02err = '0'
072400020424     c     *eur          move      G02dat        WdataJUL
072500110801     c                   endif
072600020424      *
072700020424     c                   ENDSR
072800020424
072900020424      ****************************************************************
073000020424      * DEFINIZIONE KLIST
073100020424      ****************************************************************
073200020424     c     DefKLIST      BEGSR
073300020424      *
073400020424      * file TITAS30C
073500020424     c     K04DCT02      klist
073600020424     c                   kfld                    DCTaas
073700020424     c                   kfld                    DCTlnp
073800020424     c                   kfld                    DCTnrs
073900020424     c                   kfld                    DCTnsp
074000020424      *
074100020424      * file TITA430C
074200020424     c     K05TA430      klist
074300020424     c                   kfld                    DCTaas
074400020424     c                   kfld                    DCTlnp
074500020424     c                   kfld                    DCTnrs
074600020424     c                   kfld                    DCTnsp
074700020424     c                   kfld                    TA4trc
074800020424      * file FNDET01L
074900020424     c     K02DET01      klist
075000020424     c                   kfld                    DCTaae
075100020424     c                   kfld                    DCTnev
075200020424      * file FNDCS01L
075300020424     c     K07DCS01      klist
075400020424     c                   kfld                    DCStpd
075500020424     c                   kfld                    DCSnkt
075600020424     c                   kfld                    DCSstd
075700020424     c                   kfld                    DCSdim
075800020424     c                   kfld                    DCShim
075900020424     c                   kfld                    DCSnks
076000020424     c                   kfld                    DCStrc
076100020424      * file FNDCL01L
076200020424     c     K03DCL01      klist
076300020424     c                   kfld                    DCTaac
076400020424     c                   kfld                    DCTfil
076500020424     c                   kfld                    DCTnca
076600110729      * file FNDCT01L
076700110729     C     K03DCT01      klist
076800110729     C                   KFLD                    DEXaac
076900110729     C                   KFLD                    DEXfil
077000110729     C                   KFLD                    DEXnca
077100020424      *
077200020424     c                   ENDSR
