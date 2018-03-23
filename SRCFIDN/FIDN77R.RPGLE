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
002100020424     FTITAS30C  IF   E           K DISK
002200020424     FTNTMD01L  IF   E           K DISK
002300020424     FFNDCS01L  IF   E           K DISK
002400020424     FFNDET01L  IF   E           K DISK
002500020424     FFNDCL01L  IF   E           K DISK
002600110729      *
002700110801     FWFDEX00F  UF a E           K DISK
002800020424      *
002900110729     FFIDN77P   O    E             PRINTER oflind(*IN41) usropn
003000020424
003100020424      *------------- SCHIERE
003200020424      * - Note dal file FNDCS00F
003300020424     D $NoteA          s                   like(DCSnot) dim(3) inz
003400020424      *
003500020424      *------------- DS ESTERNE
003600020424      * - Parametri
003700020424     D KPJBA         e ds
003800110729     D FIDN76DS      e ds                  inz
003900050711      *
004000050711      * - Parametri x Controllo profilo utenti
004100050711     d TIBS34ds      e ds
004200050711      * - Ds di riferimento al file esterno AZUTE00F
004300050711     d AZUTEds       e ds                  extname(AZUTE00F)
004400050711      * - Ds per dati organigramma
004500050711     d dDATIUTE      e ds
004600020424      *
004700020424      * Flag operativi - campo DCTFLO del file FNDCT00F
004800020424     D DDCT01        e ds                  inz
004900110729      *
005000110729      * Partner EuroExpress
005100110729     D FIDN40DS      e ds                  inz
005200120423      *
005300110729      * Reperimento dati anagrafici
005400110729     d TIBS69ds      e ds
005500110729     d DS_cnaco      e ds                  inz extname(CNACO00F)
005600110729     d DS_cnind      e ds                  inz extname(CNIND00F)
005700110729     d DS_cnclp      e ds                  inz extname(CNCLP00F)
005800110729     d DS_fncls      e ds                  inz extname(FNCLS00F)
005900020424      *
006000020424      * Tabelle
006100020424     D TIBS02DS      e ds                  inz
006200020424     D DTADdct       e ds                  extname(DTAD)     inz
006300020424     D                                     prefix(A)
006400020424     D DTADdet       e ds                  extname(DTAD)     inz
006500020424     D                                     prefix(E)
006600020424     D DCCH          e ds                  inz
006700160728     d dGEDdn        e ds                  qualified         inz
006800160728     d dSTD          e ds                  inz
006900020424      *
007000020424      * Ridefinizione tracciati record
007100020424     D DS_TITAS      e ds                  extname(TITAS30C) inz
007200020424     D DS_TNTMD      e ds                  extname(TNTMD00F) inz
007300020424     D DS_FNDCS      e ds                  extname(FNDCS00F) inz
007400020424     D DS_FNDCL      e ds                  extname(FNDCL00F) inz
007500020424      *
007600020424      *------------- DS DI STATO
007700020424     D STATUS         sds
007800020424     D  VTCPGM           *proc
007900020424      *
008000020424      *------------- DS INTERNE
008100020424      * - Controllo/Inversione date
008200020424     D WLBDAT          ds                  inz
008300020424     D  G02DAT                 1      8  0
008400020424     D  G02INV                 9     16  0
008500020424     D  G02ERR                17     17
008600020424     D  G02TGI                18     22  0
008700020424      * - Chiave tipo descrizione per file FNDCS01L
008800020424     D DSnumCA         ds                  inz
008900020424     D  DCTaac
009000020424     D  DCTfil
009100020424     D  DCTnca
009200020424      *
009300020424      *------------- VARIABILI
009400020424      * - data in formato AAAAMMGG
009500020424     D WdataYMD        s              8  0 inz
009600020424      * - data in formato GIULIANO
009700020424     D WdataJUL        s               d   inz datfmt(*JUL)
009800020424      * - data odierna in formato GIULIANO
009900020424     D WoggiJUL        s               d   inz datfmt(*JUL)
010000020424      * - campo di comodo per memorizzazione DCTPER
010100020424     D WdctPER         s                   inz like(DCTper)
010200110729      * - campi di comodo per salto pagina cambio  PARTNER
010300110729     D SavCod          s                       like(DEXcod)
010400120423      *
010500020424      * - indice schiera
010600020424     D XX              s              3  0 inz
010700120423      *
010800020424      * - flag record da elaborare
010900020424     D $OKrec          s              1    inz(*off)
011000020424      * - flag stampato almeno un record
011100020424     D $Stampa         s              1    inz(*off)
011200020424      *
011300020424      *------------- COSTANTI
011400020619      * - causale di chiusura C.A.
011500020619     D Wchca           c                   const(900)
011600020424
011700020424      ****************************************************************
011800020424      *                   C   I   C   L   O                          *
011900020424      ****************************************************************
012000020424
012100020424     c     *ENTRY        PLIST
012200020424     c                   PARM                    KPJBA
012300020424      *
012400020424      * operazioni iniziali
012500020424     c                   exsr      SR_INZSR
012600020424      *
012700020424      * mi posiziono con la data sul file testate C.A.
012800110801     c                   if        I76ris   = 'S'
012900020424     c                   eval      *in01    = *on
013000020424     c                   move      WdataJUL      WdctPER
013100020424     c                   else
013200020424     c                   eval      *in01    = *off
013300020424     c                   eval      WdctPER  = *all'9'
013400020424     c                   endif
013500020424     c     WdctPER       setll     FNDCT000
013600020424      *
013700020424      * ciclo di lettura del file principale
013800020424      *
013900020424do  1c                   do        *hival
014000020424      *
014100110801     c     WdctPER       reade     FNDCT000
014200020424      *
014300020424      * e.o.f.
014400020424if  2c                   if        %eof(FNDCT13L)
014500020424     c                   leave
014600020424e   2c                   endif
014700020424      *
014800020424      * selezione record
014900020424     c                   exsr      SR_SELEZ
015000020424if  2c                   if        $OKrec = *off
015100020424     c                   iter
015200020424e   2c                   endif
015300020424      *
015400020424      * recupero dei dati aggiuntivi per la stampa
015500020424     c                   exsr      SR_DATI
015600110729      *
015700110729      * scrittura
015800110729     c                   exsr      SR_WRITE
015900020424      *
016000020424e   1c                   enddo
016100110729     c                   feod      WFDEX00F
016200110729      *
016300110729      * stampa
016400120423     c                   if        I76ele <> 'F'
016500110729     c                   exsr      SR_STAMPA
016600120423     c                   endif
016700020424      *
016800020424      * Fine
016900020424     c                   exsr      SR_ENDPGM
017000020424
017100020424      ****************************************************************
017200020424      * ROUTINE INIZIALE
017300020424      ****************************************************************
017400020424     c     SR_INZSR      BEGSR
017500020424      *
017600110729     c                   movel     KPJBU         FIDN76DS
017700020424      *
017800110729      * Reperisco nome azienda
017900050711     c     *dtaara       define    §azute        AZUTEds
018000050711     c     *dtaara       define    §datiute      dDATIUTE
018100050711     c                   in(E)     *dtaara
018200050711     c                   if        %ERROR or RSUT = *blanks
018300050711     c                   clear                   Tibs34Ds
018400050711     c                   call      'TIBS34R'
018500050711     c                   parm                    Tibs34Ds
018600050711     c                   in        *dtaara
018700050711     c                   endif
018800020424      *
018900020424     c                   move      *date         WoggiJUL
019000020424      *
019100050711     c                   eval      *in41    = *on
019200020424      *
019300110801if  1c                   if        I76ris   = 'S'
019400110801     c                   eval      WdataYMD = I76dat
019500020424     c                   exsr      CnvDat
019600020424     c                   z-add     G02dat        PTdata
019700020424x   1c                   else
019800020424     c                   move      *date         PTdata
019900020424e   1c                   endif
020000160728      *
020100160728      * recupero la moneta corrente
020200160728     c                   clear                   TIBS02ds
020300160728     c                   eval      T02mod = 'C'
020400160728     c                   eval      T02sif = KNSIF
020500160728     c                   eval      T02cod = 'GED'
020600160728     c                   eval      T02ke1 = 'DANNI'
020700160728     c                   call      'TIBS02R'
020800160728     c                   parm                    KPJBA
020900160728     c                   parm                    TIBS02ds
021000160728     c                   movel     T02uni        dGEDdn
021100160728      *
021200160728      * Reperimento tabella Standards Danni
021300160728     c                   clear                   TIBS02ds
021400160728     c                   eval      T02mod = 'C'
021500160728     c                   eval      T02sif = KNSIF
021600160728     c                   eval      T02cod = 'STD'
021700160728     c                   eval      T02ke1 = '1 '
021800160728     c                   eval      T02ke2 = dGEDDN.§GEDdba
021900160728     c                   call      'TIBS02R'
022000160728     c                   parm                    KPJBA
022100160728     c                   parm                    TIBS02ds
022200160728     c                   if        T02err = *blanks
022300160728     c                   movel     T02uni        dSTD
022400160728     c                   endif
022500020424      *
022600020424     c                   ENDSR
022700020424
022800020424      ****************************************************************
022900020424      * SELEZIONE DEI RECORD DA ELABORARE
023000020424      ****************************************************************
023100020424     c     SR_SELEZ      BEGSR
023200020424      *
023300020424     c                   reset                   $OKrec
023400021120      *
023500021120      * Leggo una v.l. che prevede già:
023600021120      * - la OMIT dei record con il campo DCTper = *zeros
023700021120      * Tale campo è impostato dagli appositi programmi SOLO per:
023800021120      * - spedizioni Porto Franco, export DPD
023900021120      *              (§DCTport = 'F', §DCTtisp = 'E' e §DCTeuro = 'D')
024000021120      * - spedizioni export FEDEX    (§DCTtisp = 'E' e §DCTeuro = 'F')
024100110729      * - spedizioni export EUROEXPRES (DCTtisp = 'E' e §DCTeuro = 'X') e partner con rivalsa
024200021120      * - comunque con responsabilità PARTNER           (DCTres = 'P')
024300021120      * - e con tipo gestione "Pratica Assicurativa"    (DCTfpr = 'P')
024400020424      *
024500020424     c                   movel     DCTflo        dDCT01
024600020424      *
024700020424      * record annullati
024800020424     c     DCTatb        cabne     *blanks       EndSelez
024900110729      * Network estero non EUROEXPRESS
025000110729     c     §DCTeuro      cabne     'X'           EndSelez
025100120413      *
025200120413      * Aggancio file liquidazione C.A.
025300120413     c     K03DCL01      chain     FNDCL000
025400120413if  1c                   if        not %found(FNDCL01L)
025500120413     c                             or  DCLatb <> *blanks
025600120413     c                   clear                   DS_FNDCL
025700120413e   1c                   endif
025800120413      * importo da liquidare maggiore o uguale a 100
025900160728      * (ora tale importo è contenuto in tab. "STD": campo §STDIPDEEX)
026000160728     c*//  dclipl        cablt     100           EndSelez
026100160728     c     dclipl        cablt     §STDipdEEX    EndSelez
026200020619      * Fase 900 (se NON RIstampa)
026300110801if  1c                   if            I76ris <> 'S'
026400020619     c                             and DCTfca <> WCHCA
026500020619     c                   goto      EndSelez
026600020619e   1c                   endif
026700020424      *
026800020424      * record da elaborare
026900020424     c                   eval      $OKrec = *on
027000020424      *
027100020424     c     EndSelez      ENDSR
027200020424
027300020424      ****************************************************************
027400020424      * RECUPERO DEI DATI AGGIUNTIVI PER LA STAMPA
027500020424      ****************************************************************
027600020424     c     SR_DATI       BEGSR
027700020424      *
027800020424      * Aggancio file bolle sede
027900020424     c     K04DCT02      chain     TITAS30C
028000020424if  1c                   if        not %found(TITAS30C)
028100020424     c                   clear                   DS_TITAS
028200020424e   1c                   endif
028300020424      *
028400020424      * Aggancio file mittenti/destinatari
028500020424     c     K04DCT02      chain     TNTMD000
028600020424if  1c                   if        not %found(TNTMD01L)
028700020424     c                   clear                   DS_TNTMD
028800020424e   1c                   endif
028900020424      *
029000020424      * Aggancio testata evento danni
029100020424if  1c                   if        DCTnev > *zeros
029200020424     c                   eval      *in03  = *on
029300020424     c     K02DET01      chain     FNDET000
029400020424if  2c                   if        not %found(FNDET01L)
029500020424     c                             or  DETatb <> *blanks
029600020424     c                   clear                   DETtad
029700020424e   2c                   endif
029800020424x   1c                   else
029900020424     c                   eval      *in03  = *off
030000020424     c                   clear                   DETtad
030100020424e   1c                   endif
030200020424      *
030300020424      * Aggancio file descrizioni per descrizione anomalia
030400020424     c                   clear                   XX
030500020424     c                   clear                   $NoteA
030600020424     c                   clear                   DS_FNDCS
030700020424     c                   movel     'C'           DCStpd
030800020424     c                   movel(p)  DSnumCA       DCSnkt
030900020424     c                   movel     'A'           DCSstd
031000020424     c                   clear                   DCSdim
031100020424     c                   clear                   DCShim
031200020424     c                   clear                   DCSnks
031300020424     c                   movel     'D'           DCStrc
031400020424     c     K07DCS01      chain     FNDCS000
031500020424if  1c                   if        %found(FNDCS01L)
031600020424do  2c                   dou           %eof(FNDCS01L)
031700020424     c                             or  XX >= 3
031800020424if  3c                   if        DCSatb =  *blanks
031900020424     c                   add       1             XX
032000020424     c                   movel     DCSnot        $NoteA(XX)
032100020424e   3c                   endif
032200020424     c     K07DCS01      reade     FNDCS000
032300020424e   2c                   enddo
032400020424e   1c                   endif
032500020424      *
032600020424      * Aggancio file descrizioni per descrizione pezzi mancanti
032700020424     c                   clear                   DS_FNDCS
032800020424if  1c                   if        DCTnpz > *zeros
032900020424     c                   movel     'C'           DCStpd
033000020424     c                   movel(p)  DSnumCA       DCSnkt
033100020424     c                   movel     'P'           DCSstd
033200020424     c                   clear                   DCSdim
033300020424     c                   clear                   DCShim
033400020424     c                   clear                   DCSnks
033500020424     c                   movel     'D'           DCStrc
033600020424     c     K07DCS01      chain     FNDCS000
033700020424do  1c                   if        not %found(FNDCS01L)
033800020424     c                             or  DCSatb <> *blanks
033900020424     c                   clear                   DCSnot
034000020424e   1c                   endif
034100020424e   1c                   endif
034200020424      *
034300020424      * Decodifico tipo anomalia
034400020424     c                   clear                   TIBS02DS
034500020424     c                   movel     'TAD'         T02cod
034600020424     c                   movel(p)  DCTtad        T02ke1
034700020424     c                   exsr      CallTIBS02
034800020424     c                   movel     T02uni        DTADdct
034900020424      *
035000020424      * Decodifico evento
035100020424     c                   clear                   DTADdet
035200020424if  1c                   if        *in03
035300020424     c                   clear                   TIBS02DS
035400020424     c                   movel     'TAD'         T02cod
035500020424     c                   movel     DETtad        T02ke1
035600020424     c                   exsr      CallTIBS02
035700020424     c                   movel     T02uni        DTADdet
035800020424e   1c                   endif
035900020424      *
036000020424      * Decodifico eventuale causale di chiusura
036100020424     c                   clear                   DCCH
036200020620     c                   eval      *in04      =  *off
036300020620if  1c                   if        DCTcch   <> *blanks
036400020620     c                   eval      *in04      =  *on
036500020424     c                   clear                   TIBS02DS
036600020424     c                   movel     'CCH'         T02cod
036700020424     c                   movel(p)  DCTcch        T02ke1
036800020424     c                   exsr      CallTIBS02
036900020424     c                   movel     T02uni        DCCH
037000020424e   1c                   endif
037100020424      *
037200020424     c                   ENDSR
037300110729      *
037400110729      ****************************************************************
037500110729      * SCRIVO IL RECORD DI LAVORO
037600110729      ****************************************************************
037700110729     c     SR_WRITE      BEGSR
037800110729      *
037900110729      * IMPOSTAZIONE CAMPI DEL FILE
038000110729     c                   clear                   WFDEX000
038100120423      *
038200120423      * - Estrazione
038300120423     c                   eval      DEXute     =  KNmUs
038400120423     c                   eval      DEXdtec    =  *date
038500110729      *
038600110729      * - Avviso Presunto Danno
038700110729     c                   eval      DEXdit     =  DCTdit
038800110729     c                   eval      DEXPrn     =  DCTprn
038900110729     c                   eval      DEXpra     =  DCTpra
039000110729      * - Comunicazione Anomalia
039100110729     c                   eval      DEXfil     =  DCTfil
039200110729     c                   eval      DEXnca     =  DCTnca
039300110729     c                   eval      DEXaac     =  DCTaac
039400110729      * - Spedizione
039500110729     c                   eval      DEXlnp     =  DCTlnp
039600110729     c                   eval      DEXnrs     =  DCTnrs
039700110729     c                   eval      DEXnsp     =  DCTnsp
039800110729     c                   z-add     TASmgs        WdataYMD
039900110729     c                   movel     DCTaas        WdataYMD
040000110729     c                   exsr      CNVDAT
040100110729     c                   z-add     G02dat        DEXdsp
040200110729      * - Partner
040300110729     c                   clear                   fidn40ds
040400110801     c                   eval      I40dsp = ((TASaas*10000)+TASmgs)
040500110729     c                   If        §dcttisp = 'E'
040600110729     c                   eval      I40fil = dctlna
040700110729     c                   endif
040800110729     c                   call      'FIDN40R'
040900110729     c                   parm                    fidn40ds
041000110729     c                   If        o40cod <> *zeros
041100110729     c                   eval      DEXcod = O40cod
041200110801     c                   else
041300110801     c                   leavesr
041400110729     c                   endif
041500120502     c                   If        SavCod  = 0   or
041600120423     c                             Savcod <> DEXcod
041700120502     c                   eval      Savcod  = DEXcod
041800120423     c                   clear                   TIBS69DS
041900120423     c                   z-add     DEXcod        I69kac
042000120423     c                   CALL      'TIBS69R'
042100120423     c                   PARM                    tibs69DS
042200120423     c                   PARM                    DS_cnaco
042300120423     c                   PARM                    DS_cnind
042400120423     c                   PARM                    DS_cnclp
042500120423     c                   PARM                    DS_fncls
042600120502     c                   EndIf
042700120423     c                   If        o69err = ' '
042800120423     c                   movel     ACOrag        DEXrag
042900120502     c                   Else
043000120423     c                   eval      DEXrag = *all'?'
043100120502     c                   EndIf
043200110729      * - Mittente
043300110729     c                   movel     TMDrsm        DEXrsm
043400110729     c                   movel     TMDinm        DEXinm
043500110729     c                   movel     TMDcam        DEXcam
043600110729     c                   movel     TMDlom        DEXlom
043700110729     c                   movel     TMDprm        DEXprm
043800110801     c                   movel     TMDnzm        DEXnam
043900110729      * - Destinatario
044000110729     c                   movel     TMDrsd        DEXrsd
044100110729     c                   movel     TMDind        DEXind
044200110729     c                   movel     TMDcad        DEXcad
044300110729     c                   movel     TMDlod        DEXlod
044400110729     c                   movel     TMDprd        DEXprd
044500110801     c                   movel     TMDnzd        DEXnad
044600110729      *
044700110729      * - Colli in bolla
044800110729     c                   eval      DEXncl     =  TASncl
044900110729      * - Peso Kg in bolla
045000110729     c                   eval      DEXpkf     =  TASpkf
045100110729      * - Evento
045200110729if  1c                   if        *in03
045300110729     c                   eval      DEXaae = DCTaae
045400110729     c                   eval      DEXnev = DCTnev
045500110729if  2c                   if        E§TADdest  <> *blanks
045600110729     c                   movel     E§TADdest     DEXtae
045700110729x   2c                   else
045800110729     c                   movel     E§TADdesc     DEXtae
045900110729e   2c                   endif
046000110729e   1c                   endif
046100110729      *
046200110729      * Campi del rec. DN77PD3:
046300110729      * - Colli danneggiati/mancanti
046400110729     c                   eval      DEXncn     =  DCTncn
046500110729      * - Peso Kg danneggiato/mancanti
046600110729     c                   eval      DEXpkd     =  DCTpkd
046700110729      * - Pezzi danneggiati/mancanti
046800110729     c                   eval      DEXnpz     =  DCTnpz
046900110729      * - Note
047000110729     c                   movel     DCSnot        DEXdpz
047100110729      *
047200110729      * Campi del rec. DN77PD4:
047300110729      * - Anomalia
047400110729if  1c                   if        A§TADdest  <> *blanks
047500110729     c                   movel     A§TADdest     DEXtad
047600110729x   1c                   else
047700110729     c                   movel     A§TADdesc     DEXtad
047800110729e   1c                   endif
047900110729      * - Note aggiuntive (descrizione anomalia)
048000110729     c                   movel     $NoteA(1)     DEXdan
048100110729     c                   movel(p)  $NoteA(2)     DEXkos
048200110729     c                   move      $NoteA(3)     DEXkos
048300110729      *
048400110729      * - Importo rimborsato
048500110729if  1c                   if        DCLipl < DCLipt
048600110729     c                   eval      DEXipr = 'VEDI C.A.'
048700110729x   1c                   else
048800110729     c                   eval      DEXipr = %trim(%editc(DCLipl:'K'))
048900110729e   1c                   endif
049000110729      * - Causale di chiusura
049100110729if  1c                   if        DCTcch     <> *blanks
049200110729     c                   eval      *in04      =  *on
049300110729     c                   eval      DEXcch     =  DCTcch
049400110729     c                   eval      DEXccd     =  §CCHdesc
049500110729x   1c                   else
049600110729     c                   clear                   DEXcch
049700110729     c                   clear                   DEXccd
049800110729e   1c                   endif
049900110729      *
050000110729      * SCRITTURA DLE RECORD
050100110729     C                   WRITE     WFDEX000
050200110729      *
050300110729     c                   ENDSR
050400020424
050500020424      ****************************************************************
050600110729      * STAMPA IL RECORD DI LAVORO
050700020424      ****************************************************************
050800110729     c     SR_STAMPA     BEGSR
050900020424      *
051000110729      * Lettura del file di lavoro
051100120423     c                   clear                   Savcod
051200110729     c     *loval        setll     WFdex00f
051300110729     c                   do        *hival
051400110729     c                   read      WFdex00f
051500110729     c                   If        %eof(WFdex00f)
051600110729     c                   leave
051700110729     c                   endif
051800110729      * verifico se cambiato partner
051900110729     c                   If        SavCod = 0 or
052000110729     c                             Savcod <> DEXcod
052100110729     c                   eval      Savcod = DEXcod
052200110801     c                   eval      PTDcod = DEXcod
052300120423     c                   eval      PTDdes = DEXrag
052400110729     c                   eval      *in41 = *on
052500110801     c                   endif
052600020424      *
052700110729      * Campi del rec. DN77PD1:
052800020424      * - Avviso Presunto Danno
052900110729     c                   if           DEXdit  <> *blanks
053000110729     c                             or DEXprn  <> *zeros
053100110729     c                             or DEXpra  <> *zeros
053200020424     c                   eval      *in02      =  *on
053300110729     c                   eval      PD1dit     =  DEXdit
053400110729     c                   eval      PD1prn     =  DEXprn
053500110729     c                   eval      PD1pra     =  DEXpra
053600020424     c                   else
053700020424     c                   eval      *in02      =  *off
053800020424     c                   endif
053900020424      * - Comunicazione Anomalia
054000110729     c                   eval      PD1fil     =  DEXfil
054100110729     c                   eval      PD1nca     =  DEXnca
054200110729     c                   eval      PD1aac     =  DEXaac
054300020424      * - Spedizione
054400110729     c                   eval      PD1lnp     =  DEXlnp
054500110729     c                   eval      PD1nrs     =  DEXnrs
054600110729     c                   eval      PD1nsp     =  DEXnsp
054700110729     c                   z-add     DEXDSP        PD1dsp
054800020424      * - Mittente
054900110729     c                   movel     DEXrsm        PD1rsm
055000110729     c                   movel     DEXinm        PD1inm
055100110729     c                   movel     DEXcam        PD1cam
055200110729     c                   movel     DEXlom        PD1lom
055300110729     c                   movel     DEXprm        PD1prm
055400110801     c                   movel     DEXnam        PD1nzm
055500020424      * - Destinatario
055600110729     c                   movel     DEXrsd        PD1rsd
055700110729     c                   movel     DEXind        PD1ind
055800110729     c                   movel     DEXcad        PD1cad
055900110729     c                   movel     DEXlod        PD1lod
056000110729     c                   movel     DEXprd        PD1prd
056100110801     c                   movel     DEXnad        PD1nzd
056200020424      *
056300110729      * Campi del rec. DN77PD2:
056400020424      * - Colli in bolla
056500110729     c                   eval      PD2ncl     =  DEXncl
056600020424      * - Peso Kg in bolla
056700110729     c                   eval      PD2pkf     =  DEXpkf
056800020424      * - Evento
056900110729     c                   if        dexnev > 0
057000110729if  1c                   eval      *in03 = *on
057100110729     c                   eval      PD2kev     =  %editc(DEXaae:'Z') + '/' +
057200110729     c                                        %trim(%editc(DEXnev:'Z'))
057300110729     c                   movel     DEXtae        PD2tae
057400110729     c                   else
057500110729if  1c                   eval      *in03 = *off
057600020424e   1c                   endif
057700020424      *
057800110729      * Campi del rec. DN77PD3:
057900020424      * - Colli danneggiati/mancanti
058000110729     c                   eval      PD3ncn     =  DEXncn
058100020424      * - Peso Kg danneggiato/mancanti
058200110729     c                   eval      PD3pkd     =  DEXpkd
058300020424      * - Pezzi danneggiati/mancanti
058400110729     c                   eval      PD3npz     =  DEXnpz
058500020424      * - Note
058600110801     c                   movel     DCSnot        DEXdpz
058700020424      *
058800110729      * Campi del rec. DN77PD4:
058900020424      * - Anomalia
059000110729     c                   movel     DEXtad        PD4tad
059100020424      * - Note aggiuntive (descrizione anomalia)
059200110729     c                   movel     DEXdan        PD4dan
059300110729     c                   movel     DEXkos        PD4kos
059400020424      *
059500110729      * Campi del rec. DN77PD5:
059600020620      * - Importo rimborsato
059700110729     c                   eval      PD5ipr = DEXipr
059800020424      * - Causale di chiusura
059900110729if  1c                   if        DEXcch     <> *blanks
060000020620     c                   eval      *in04      =  *on
060100110729     c                   eval      PD5cch     =  DEXcch
060200110729     c                   eval      PD5ccd     =  DEXccd
060300020424x   1c                   else
060400020620     c                   clear                   PD5cch
060500020620     c                   clear                   PD5ccd
060600020424e   1c                   endif
060700020424      *
060800020424      * STAMPA
060900020620      *
061000020424      * - apertura iniziale del prtf
061100020424if  2c                   if        $Stampa    =  *off
061200110729     c                   open      FIDN77P
061300020424e   2c                   endif
061400120416      * - separatore
061500120416     c                   if        $Stampa    =  *on
061600120416     c                   write     DN77P1
061700120416     c                   endif
061800020424      * - testata
061900050711     c                   if        *IN41
062000110729     c                   write     DN77PT
062100020424     c                   eval      *IN41      =  *off
062200020424     c                   endif
062300020424      * - dettaglio
062400110729     c                   write     DN77PD1
062500110729     c                   write     DN77PD2
062600110729     c                   write     DN77PD3
062700110729     c                   write     DN77PD4
062800110729     c                   write     DN77PD5
062900020424     c                   eval      $Stampa    =  *on
063000020620      *
063100020424      *
063200020424      * AGGIORNAMENTO DEL RECORD IN ESAME DEL FILE FNDCT13L
063300050711      *   (se NON ristampa)
063400020424      *
063500110801     c                   if        I76ris     =  *blanks
063600110729     c     K03DCT01      chain     fndct01l
063700110729     c                   If        %found(fndct01l)
063800020424     c                   move      WoggiJUL      DCTper
063900110729     c                   update    FNDCT01
064000110729     c                   endif
064100020424     c                   endif
064200020424      *
064300110801     c                   enddo
064400110801      *
064500020424     c                   ENDSR
064600020424
064700020424      ****************************************************************
064800020424      * ROUTINE FINALE
064900020424      ****************************************************************
065000020424     c     SR_ENDPGM     BEGSR
065100020424      *
065200020424      * Segnalo la mancanza di dati da stampare
065300120424if  1c                   if             $Stampa  = *off
065400120424     c                             and  I76ele  <> 'F'
065500110729     c                   open      FIDN77P
065600110729     c                   write     DN77PT
065700110729     c                   write     DN77PD0
065800020424     c                   eval      $Stampa  = *on
065900020424e   1c                   endif
066000020424      *
066100020424      * Stampo "Fine Stampa"
066200020424if  1c                   if        $Stampa  = *on
066300110729     c                   write     DN77PE
066400020424e   1c                   endif
066500020424      *
066600020424      * Chiudo TIBS02R
066700020424     c                   clear                   TIBS02DS
066800020424     c                   movel     'C'           T02tla
066900020424     c                   call      'TIBS02R'
067000020424     c                   parm                    KPJBA
067100020424     c                   parm                    TIBS02DS
067200020424      *
067300020424      * Chiudo archivi aperti
067400020424     c                   close     FNDCT13L
067500020424     c                   if        $Stampa  = *on
067600110729     c                   close     FIDN77P
067700020424     c                   eval      $Stampa  = *off
067800020424     c                   endif
067900020424      *
068000020424      * Termino !
068100020424     c                   eval      *INLR    = *on
068200020424     c                   return
068300020424      *
068400020424     c                   ENDSR
068500020424
068600020424      ****************************************************************
068700020424      * RICHIAMO PGM RICERCA/CONTROLLO TABELLE
068800020424      ****************************************************************
068900020424     c     CallTIBS02    BEGSR
069000020424      *
069100020424     c                   movel     'C'           T02mod
069200020424     c                   movel     KNSIF         T02sif
069300020424      *
069400020424     c                   call      'TIBS02R'
069500020424     c                   parm                    KPJBA
069600020424     c                   parm                    TIBS02DS
069700020424      *
069800020424     c                   ENDSR
069900020424
070000020424      ****************************************************************
070100020424      * CONVERSIONE DATA DAL FORMATO AAAA/MM/GG AL FORMATO AA/GGG
070200020424      ****************************************************************
070300020424     c     CNVDAT        BEGSR
070400020424      *
070500020424     c                   clear                   WLBdat
070600020424     c                   eval      G02inv = WdataYMD
070700020424     c                   eval      G02err = '3'
070800020424     c                   call      'XSRDA8'
070900020424     c                   parm                    WLBDAT
071000020424      *
071100110801     c                   If        g02err = '0'
071200020424     c     *eur          move      G02dat        WdataJUL
071300110801     c                   endif
071400020424      *
071500020424     c                   ENDSR
071600020424
071700020424      ****************************************************************
071800020424      * DEFINIZIONE KLIST
071900020424      ****************************************************************
072000020424     c     DefKLIST      BEGSR
072100020424      *
072200020424      * file TITAS30C
072300020424     c     K04DCT02      klist
072400020424     c                   kfld                    DCTaas
072500020424     c                   kfld                    DCTlnp
072600020424     c                   kfld                    DCTnrs
072700020424     c                   kfld                    DCTnsp
072800020424      *
072900020424      * file FNDET01L
073000020424     c     K02DET01      klist
073100020424     c                   kfld                    DCTaae
073200020424     c                   kfld                    DCTnev
073300020424      * file FNDCS01L
073400020424     c     K07DCS01      klist
073500020424     c                   kfld                    DCStpd
073600020424     c                   kfld                    DCSnkt
073700020424     c                   kfld                    DCSstd
073800020424     c                   kfld                    DCSdim
073900020424     c                   kfld                    DCShim
074000020424     c                   kfld                    DCSnks
074100020424     c                   kfld                    DCStrc
074200020424      * file FNDCL01L
074300020424     c     K03DCL01      klist
074400020424     c                   kfld                    DCTaac
074500020424     c                   kfld                    DCTfil
074600020424     c                   kfld                    DCTnca
074700110729      * file FNDCT01L
074800110729     C     K03DCT01      klist
074900110729     C                   KFLD                    DEXaac
075000110729     C                   KFLD                    DEXfil
075100110729     C                   KFLD                    DEXnca
075200020424      *
075300020424     c                   ENDSR
