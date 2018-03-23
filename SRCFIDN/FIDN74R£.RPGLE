000100021121      ****************************************************************
000200021121      *    Elenco C.A. per rivalsa FEDEX   -   batch                 *
000300021121      ****************************************************************
000400021121
000500021121      ****************************************************************
000600021121      *  RIEPILOGO INDICATORI                                        *
000700021121      *--------------------------------------------------------------*
000800021121      * 01 - RISTAMPA                                                *
000900021121      * 02 - STAMPO Avviso Danno                                     *
001000021121      * 03 - STAMPO Evento                                           *
001100021121      * 04 - STAMPO Causale Chiusura                                 *
001200021121      * 41 - EOF FIDN74P                                             *
001300021121      ****************************************************************
001400021121
001500021121     H DECEDIT('0,') DATEDIT(*DMY.)
001600021121
001700021121     FFNDCT13L  UF   E           K DISK
001800021121     FTITA430C  IF   E           K DISK
001900021121     FTITAS30C  IF   E           K DISK
002000021121     FTNTMD01L  IF   E           K DISK
002100021121     FFNDCS01L  IF   E           K DISK
002200021121     FFNDET01L  IF   E           K DISK
002300021121     FFNDCL01L  IF   E           K DISK
002400021121      *
002500021121     FFIDN74P   O    E             PRINTER oflind(*IN41) usropn
002600021121
002700021121      *------------- SCHIERE
002800021121      * - Note dal file FNDCS00F
002900021121     D $NoteA          s                   like(DCSnot) dim(3) inz
003000021121      *
003100021121      *------------- DS ESTERNE
003200021121      * - Parametri
003300021121     D KPJBA         e ds
003400021121     D FIDN73DS      e ds                  inz
003500050711      *
003600050711      * - Parametri x Controllo profilo utenti
003700050711     d TIBS34ds      e ds
003800050711      * - Ds di riferimento al file esterno AZUTE00F
003900050711     d AZUTEds       e ds                  extname(AZUTE00F)
004000050711      * - Ds per dati organigramma
004100050711     d dDATIUTE      e ds
004200021121      *
004300070927     D DSbl4E        e ds                  inz
004400021121      *
004500021121      * Flag operativi - campo DCTFLO del file FNDCT00F
004600021121     D DDCT01        e ds                  inz
004700021121      *
004800021121      * Tabelle
004900021121     D TIBS02DS      e ds                  inz
005000021121     D DTADdct       e ds                  extname(DTAD)     inz
005100021121     D                                     prefix(A)
005200021121     D DTADdet       e ds                  extname(DTAD)     inz
005300021121     D                                     prefix(E)
005400021121     D DCCH          e ds                  inz
005500021121      *
005600021121      * Ridefinizione tracciati record
005700021121     D DS_TITAS      e ds                  extname(TITAS30C) inz
005800021121     D DS_TNTMD      e ds                  extname(TNTMD00F) inz
005900021121     D DS_FNDCS      e ds                  extname(FNDCS00F) inz
006000021121     D DS_FNDCL      e ds                  extname(FNDCL00F) inz
006100021121      *
006200021121      *------------- DS DI STATO
006300021121     D STATUS         sds
006400021121     D  VTCPGM           *proc
006500021121      *
006600021121      *------------- DS INTERNE
006700021121      * - Controllo/Inversione date
006800021121     D WLBDAT          ds                  inz
006900021121     D  G02DAT                 1      8  0
007000021121     D  G02INV                 9     16  0
007100021121     D  G02ERR                17     17
007200021121     D  G02TGI                18     22  0
007300021121      * - Chiave tipo descrizione per file FNDCS01L
007400021121     D DSnumCA         ds                  inz
007500021121     D  DCTaac
007600021121     D  DCTfil
007700021121     D  DCTnca
007800021121      *
007900021121      *------------- VARIABILI
008000021121      * - data in formato AAAAMMGG
008100021121     D WdataYMD        s              8  0 inz
008200021121      * - data in formato GIULIANO
008300021121     D WdataJUL        s               d   inz datfmt(*JUL)
008400021121      * - data odierna in formato GIULIANO
008500021121     D WoggiJUL        s               d   inz datfmt(*JUL)
008600021121      * - campo di comodo per memorizzazione DCTPER
008700021121     D WdctPER         s                   inz like(DCTper)
008800021121      * - campi di comodo per QCMDEXC
008900021121     D Wcommand        s            100    inz
009000021121     D Wlung           s             15  5 inz(100)
009100021121      * - indice schiera
009200021121     D XX              s              3  0 inz
009300021121      * - flag record da elaborare
009400021121     D $OKrec          s              1    inz(*off)
009500021121      * - flag stampato almeno un record
009600021121     D $Stampa         s              1    inz(*off)
009700021121      *
009800021121      *------------- COSTANTI
009900021121      * - causale di chiusura C.A.
010000021121     D Wchca           c                   const(900)
010100021121
010200021121      ****************************************************************
010300021121      *                   C   I   C   L   O                          *
010400021121      ****************************************************************
010500021121
010600021121     c     *ENTRY        PLIST
010700021121     c                   PARM                    KPJBA
010800021121      *
010900021121      * operazioni iniziali
011000021121     c                   exsr      SR_INZSR
011100021121      *
011200021121      * mi posiziono con la data sul file testate C.A.
011300021121     c                   if        I73ris   = 'S'
011400021121     c                   eval      *in01    = *on
011500021121     c                   move      WdataJUL      WdctPER
011600021121     c                   else
011700021121     c                   eval      *in01    = *off
011800021121     c                   eval      WdctPER  = *all'9'
011900021121     c                   endif
012000021121     c     WdctPER       setll     FNDCT000
012100021121      *
012200021121      * ciclo di lettura del file principale
012300021121      *
012400021121do  1c                   do        *hival
012500021121      *
012600021121     c     WdctPER       reade     FNDCT000
012700021121      *
012800021121      * e.o.f.
012900021121if  2c                   if        %eof(FNDCT13L)
013000021121     c                   leave
013100021121e   2c                   endif
013200021121      *
013300021121      * selezione record
013400021121     c                   exsr      SR_SELEZ
013500021121if  2c                   if        $OKrec = *off
013600021121     c                   iter
013700021121e   2c                   endif
013800021121      *
013900021121      * recupero dei dati aggiuntivi per la stampa
014000021121     c                   exsr      SR_DATI
014100021121      *
014200021121      * stampa
014300021121     c                   exsr      SR_STAMPA
014400021121      *
014500021121e   1c                   enddo
014600021121      *
014700021121      * Fine
014800021121     c                   exsr      SR_ENDPGM
014900021121
015000021121      ****************************************************************
015100021121      * ROUTINE INIZIALE
015200021121      ****************************************************************
015300021121     c     SR_INZSR      BEGSR
015400021121      *
015500021121     c                   movel     KPJBU         FIDN73DS
015600021121      *
015700050711      * Reperisco nome azienda per stampa PREPAGATI
015800050711     c     *dtaara       define    §azute        AZUTEds
015900050711     c     *dtaara       define    §datiute      dDATIUTE
016000050711     c                   in(E)     *dtaara
016100050711     c                   if        %ERROR or RSUT = *blanks
016200050711     c                   clear                   Tibs34Ds
016300050711     c                   call      'TIBS34R'
016400050711     c                   parm                    Tibs34Ds
016500050711     c                   in        *dtaara
016600050711     c                   endif
016700021121      *
016800021121     c                   move      *date         WoggiJUL
016900050711      *
017000050711     c                   eval      *in41    = *on
017100021121      *
017200021121if  1c                   if        I73ris   = 'S'
017300021121     c                   eval      WdataYMD = I73dat
017400021121     c                   exsr      CnvDat
017500021121     c                   z-add     G02dat        PTdata
017600021121     c***                move      WdataJUL      WdctPER
017700021121x   1c                   else
017800021121     c                   move      *date         PTdata
017900021121e   1c                   endif
018000021121      *
018100021121     c                   ENDSR
018200021121
018300021121      ****************************************************************
018400021121      * SELEZIONE DEI RECORD DA ELABORARE
018500021121      ****************************************************************
018600021121     c     SR_SELEZ      BEGSR
018700021121      *
018800021121     c                   reset                   $OKrec
018900021121      *
019000021121      * Leggo una v.l. che prevede già:
019100021121      * - la OMIT dei record con il campo DCTper = *zeros
019200021121      * Tale campo è impostato dagli appositi programmi SOLO per:
019300021121      * - spedizioni Porto Franco, export DPD
019400021121      *              (§DCTport = 'F', §DCTtisp = 'E' e §DCTeuro = 'D')
019500021121      * - spedizioni export FEDEX    (§DCTtisp = 'E' e §DCTeuro = 'F')
019600021121      * - comunque con responsabilità PARTNER           (DCTres = 'P')
019700021121      * - e con tipo gestione "Pratica Assicurativa"    (DCTfpr = 'P')
019800021121      *
019900021121     c                   movel     DCTflo        dDCT01
020000021121      *
020100021121      * record annullati
020200021121     c     DCTatb        cabne     *blanks       EndSelez
020300021121      * Network estero non FedEx
020400021121     c     §DCTeuro      cabne     'F'           EndSelez
020500021121      * Fase 900 (se NON RIstampa)
020600021121if  1c                   if            I73ris <> 'S'
020700021121     c                             and DCTfca <> WCHCA
020800021121     c                   goto      EndSelez
020900021121e   1c                   endif
021000021121      *
021100021121      * record da elaborare
021200021121     c                   eval      $OKrec = *on
021300021121      *
021400021121     c     EndSelez      ENDSR
021500021121
021600021121      ****************************************************************
021700021121      * RECUPERO DEI DATI AGGIUNTIVI PER LA STAMPA
021800021121      ****************************************************************
021900021121     c     SR_DATI       BEGSR
022000021121      *
022100021121      * Aggancio file bolle sede
022200021121     c     K04DCT02      chain     TITAS30C
022300021121if  1c                   if        not %found(TITAS30C)
022400021121     c                   clear                   DS_TITAS
022500021121e   1c                   endif
022600021121      *
022700021121      * Aggancio file riferimenti bolle
022800070927     c***                eval      TA4trc = 'A'
022900070927     c***  K05TA430      chain     TITA430C
023000070927if  1c***                if        %found(TITA430C)
023100070927     c***                movel     TA4not        DTA4A
023200070927x   1c***                else
023300070927     c***                clear                   DTA4A
023400070927e   1c***                endif
023500070927     c
023600070927     c                   eval      TA4trc = 'E'
023700070927     c     K05TA430      chain     TITA430C
023800070927if  1c                   if        %found(TITA430C)
023900070927     c                   movel     TA4not        dsbl4e
024000070927x   1c                   else
024100070927     c                   clear                   dsbl4e
024200070927e   1c                   endif
024300021121      *
024400021121      * Aggancio file mittenti/destinatari
024500021121     c     K04DCT02      chain     TNTMD000
024600021121if  1c                   if        not %found(TNTMD01L)
024700021121     c                   clear                   DS_TNTMD
024800021121e   1c                   endif
024900021121      *
025000021121      * Aggancio testata evento danni
025100021121if  1c                   if        DCTnev > *zeros
025200021121     c                   eval      *in03  = *on
025300021121     c     K02DET01      chain     FNDET000
025400021121if  2c                   if        not %found(FNDET01L)
025500021121     c                             or  DETatb <> *blanks
025600021121     c                   clear                   DETtad
025700021121e   2c                   endif
025800021121x   1c                   else
025900021121     c                   eval      *in03  = *off
026000021121     c                   clear                   DETtad
026100021121e   1c                   endif
026200021121      *
026300021121      * Aggancio file descrizioni per descrizione anomalia
026400021121     c                   clear                   XX
026500021121     c                   clear                   $NoteA
026600021121     c                   clear                   DS_FNDCS
026700021121     c                   movel     'C'           DCStpd
026800021121     c                   movel(p)  DSnumCA       DCSnkt
026900021121     c                   movel     'A'           DCSstd
027000021121     c                   clear                   DCSdim
027100021121     c                   clear                   DCShim
027200021121     c                   clear                   DCSnks
027300021121     c                   movel     'D'           DCStrc
027400021121     c     K07DCS01      chain     FNDCS000
027500021121if  1c                   if        %found(FNDCS01L)
027600021121do  2c                   dou           %eof(FNDCS01L)
027700021121     c                             or  XX >= 3
027800021121if  3c                   if        DCSatb =  *blanks
027900021121     c                   add       1             XX
028000021121     c                   movel     DCSnot        $NoteA(XX)
028100021121e   3c                   endif
028200021121     c     K07DCS01      reade     FNDCS000
028300021121e   2c                   enddo
028400021121e   1c                   endif
028500021121      *
028600021121      * Aggancio file descrizioni per descrizione pezzi mancanti
028700021121     c                   clear                   DS_FNDCS
028800021121if  1c                   if        DCTnpz > *zeros
028900021121     c                   movel     'C'           DCStpd
029000021121     c                   movel(p)  DSnumCA       DCSnkt
029100021121     c                   movel     'P'           DCSstd
029200021121     c                   clear                   DCSdim
029300021121     c                   clear                   DCShim
029400021121     c                   clear                   DCSnks
029500021121     c                   movel     'D'           DCStrc
029600021121     c     K07DCS01      chain     FNDCS000
029700021121do  1c                   if        not %found(FNDCS01L)
029800021121     c                             or  DCSatb <> *blanks
029900021121     c                   clear                   DCSnot
030000021121e   1c                   endif
030100021121e   1c                   endif
030200021121      *
030300021121      * Aggancio file liquidazione C.A.
030400021121     c     K03DCL01      chain     FNDCL000
030500021121if  1c                   if        not %found(FNDCL01L)
030600021121     c                             or  DCLatb <> *blanks
030700021121     c                   clear                   DS_FNDCL
030800021121e   1c                   endif
030900021121      *
031000021121      * Decodifico tipo anomalia
031100021121     c                   clear                   TIBS02DS
031200021121     c                   movel     'TAD'         T02cod
031300021121     c                   movel(p)  DCTtad        T02ke1
031400021121     c                   exsr      CallTIBS02
031500021121     c                   movel     T02uni        DTADdct
031600021121      *
031700021121      * Decodifico evento
031800021121     c                   clear                   DTADdet
031900021121if  1c                   if        *in03
032000021121     c                   clear                   TIBS02DS
032100021121     c                   movel     'TAD'         T02cod
032200021121     c                   movel     DETtad        T02ke1
032300021121     c                   exsr      CallTIBS02
032400021121     c                   movel     T02uni        DTADdet
032500021121e   1c                   endif
032600021121      *
032700021121      * Decodifico eventuale causale di chiusura
032800021121     c                   clear                   DCCH
032900021121     c                   eval      *in04      =  *off
033000021121if  1c                   if        DCTcch   <> *blanks
033100021121     c                   eval      *in04      =  *on
033200021121     c                   clear                   TIBS02DS
033300021121     c                   movel     'CCH'         T02cod
033400021121     c                   movel(p)  DCTcch        T02ke1
033500021121     c                   exsr      CallTIBS02
033600021121     c                   movel     T02uni        DCCH
033700021121e   1c                   endif
033800021121      *
033900021121     c                   ENDSR
034000021121
034100021121      ****************************************************************
034200021121      * STAMPA
034300021121      ****************************************************************
034400021121     c     SR_STAMPA     BEGSR
034500021121      *
034600021121      * IMPOSTAZIONE CAMPI IN STAMPA
034700021121      *
034800021121      * Campi del rec. DN74PD1:
034900021121      * - Avviso Presunto Danno
035000021121     c                   if           DCTdit  <> *blanks
035100021121     c                             or DCTprn  <> *zeros
035200021121     c                             or DCTpra  <> *zeros
035300021121     c                   eval      *in02      =  *on
035400021121     c                   eval      PD1dit     =  DCTdit
035500021121     c                   eval      PD1prn     =  DCTprn
035600021121     c                   eval      PD1pra     =  DCTpra
035700021121     c                   else
035800021121     c                   eval      *in02      =  *off
035900021121     c                   endif
036000021121      * - Comunicazione Anomalia
036100021121     c                   eval      PD1fil     =  DCTfil
036200021121     c                   eval      PD1nca     =  DCTnca
036300021121     c                   eval      PD1aac     =  DCTaac
036400021121      * - Riferimento FedEx - Numero AWB
036500070927     c****               eval      PD1nrawb   =  §TA4arpt
036600070927     c                   movel     §b4erp        PD1nrawb
036700021121      * - Spedizione
036800021121     c                   eval      PD1lnp     =  DCTlnp
036900021121     c                   eval      PD1nrs     =  DCTnrs
037000021121     c                   eval      PD1nsp     =  DCTnsp
037100021121     c                   z-add     TASmgs        WdataYMD
037200021121     c                   movel     DCTaas        WdataYMD
037300021121     c                   exsr      CNVDAT
037400021121     c                   z-add     G02dat        PD1dsp
037500021121      * - Mittente
037600021121     c                   movel     TMDrsm        PD1rsm
037700021121     c                   movel     TMDinm        PD1inm
037800021121     c                   movel     TMDcam        PD1cam
037900021121     c                   movel     TMDlom        PD1lom
038000021121     c                   movel     TMDprm        PD1prm
038100021121     c                   movel     TMDnzm        PD1nzm
038200021121      * - Destinatario
038300021121     c                   movel     TMDrsd        PD1rsd
038400021121     c                   movel     TMDind        PD1ind
038500021121     c                   movel     TMDcad        PD1cad
038600021121     c                   movel     TMDlod        PD1lod
038700021121     c                   movel     TMDprd        PD1prd
038800021121     c                   movel     TMDnzd        PD1nzd
038900021121      *
039000021121      * Campi del rec. DN74PD2:
039100021121      * - Colli in bolla
039200021121     c                   eval      PD2ncl     =  TASncl
039300021121      * - Peso Kg in bolla
039400021121     c                   eval      PD2pkf     =  TASpkf
039500021121      * - Evento
039600021121if  1c                   if        *in03
039700021121     c                   eval      PD2kev     =  %editc(DCTpra:'Z') + '/' +
039800021121     c                                        %trim(%editc(DCTnca:'Z'))
039900021121if  2c                   if        E§TADdest  <> *blanks
040000021121     c                   movel     E§TADdest     PD2tae
040100021121x   2c                   else
040200021121     c                   movel     E§TADdesc     PD2tae
040300021121e   2c                   endif
040400021121e   1c                   endif
040500021121      *
040600021121      * Campi del rec. DN74PD3:
040700021121      * - Colli danneggiati/mancanti
040800021121     c                   eval      PD3ncn     =  DCTncn
040900021121      * - Peso Kg danneggiato/mancanti
041000021121     c                   eval      PD3pkd     =  DCTpkd
041100021121      * - Pezzi danneggiati/mancanti
041200021121     c                   eval      PD3npz     =  DCTnpz
041300021121      * - Note
041400021121     c                   movel     DCSnot        PD3dpz
041500021121      *
041600021121      * Campi del rec. DN74PD4:
041700021121      * - Anomalia
041800021121if  1c                   if        A§TADdest  <> *blanks
041900021121     c                   movel     A§TADdest     PD4tad
042000021121x   1c                   else
042100021121     c                   movel     A§TADdesc     PD4tad
042200021121e   1c                   endif
042300021121      * - Note aggiuntive (descrizione anomalia)
042400021121     c                   movel     $NoteA(1)     PD4dan
042500021121     c                   movel(p)  $NoteA(2)     PD4kos
042600021121     c                   move      $NoteA(3)     PD4kos
042700021121      *
042800021121      * Campi del rec. DN74PD5:
042900021121      * - Importo rimborsato
043000030409     c***                eval      PD5ipr = %trim(%editc(DCLipl+DCLipt:'K'))
043100030409     c***                                 + ' ' + DCLvpl
043200030409if  1c                   if        DCLipl < DCLipt
043300030409     c                   eval      PD5ipr = 'VEDI C.A.'
043400030409x   1c                   else
043500030409     c                   eval      PD5ipr = %trim(%editc(DCLipl:'K'))
043600030409     c                                    + ' ' + DCLvpl
043700030409e   1c                   endif
043800021121      * - Causale di chiusura
043900021121if  1c                   if        DCTcch     <> *blanks
044000021121     c                   eval      *in04      =  *on
044100021121     c                   eval      PD5cch     =  DCTcch
044200021121     c                   eval      PD5ccd     =  §CCHdesc
044300021121x   1c                   else
044400021121     c                   clear                   PD5cch
044500021121     c                   clear                   PD5ccd
044600021121e   1c                   endif
044700021121      *
044800021121      * STAMPA
044900021121      *
045000021121      * - apertura iniziale del prtf
045100021121if  2c                   if        $Stampa    =  *off
045200021121     c                   open      FIDN74P
045300021121e   2c                   endif
045400021121      * - testata
045500021121if  2c                   if        *IN41
045600021121     c                   write     DN74PT
045700021121     c                   eval      *IN41      =  *off
045800021121     c                   endif
045900021121      * - separatore
046000021121if  2c                   if        $Stampa    =  *on
046100021121     c                   write     DN74P1
046200021121     c                   endif
046300021121      * - dettaglio
046400021121     c                   write     DN74PD1
046500021121     c                   write     DN74PD2
046600021121     c                   write     DN74PD3
046700021121     c                   write     DN74PD4
046800021121     c                   write     DN74PD5
046900021121     c                   eval      $Stampa    =  *on
047000021121      *
047100021121      * AGGIORNAMENTO DEL RECORD IN ESAME DEL FILE FNDCT13L
047200021121      *   (se NON ristampa)
047300021121      *
047400021121     c                   if        I73ris     =  *blanks
047500021121     c                   move      WoggiJUL      DCTper
047600021121     c                   update    FNDCT000
047700021121     c                   endif
047800021121      *
047900021121     c                   ENDSR
048000021121
048100021121      ****************************************************************
048200021121      * ROUTINE FINALE
048300021121      ****************************************************************
048400021121     c     SR_ENDPGM     BEGSR
048500021121      *
048600021121      * Segnalo la mancanza di dati da stampare
048700021121      * - Pratiche Assicurative NON in Franchigia (per Consuldanni)
048800050711if  1c                   if        $Stampa  = *off
048900021121     c                   open      FIDN74P
049000021121     c                   eval      *in09    = *off
049100021121     c                   write     DN74PT
049200021121     c                   write     DN74PD0
049300021121     c                   eval      $Stampa  = *on
049400021121e   1c                   endif
049500021121      *
049600021121      * Stampo "Fine Stampa"
049700021121if  1c                   if        $Stampa  = *on
049800021121     c                   write     DN74PE
049900021121e   1c                   endif
050000021121      *
050100021121      * Chiudo TIBS02R
050200021121     c                   clear                   TIBS02DS
050300021121     c                   movel     'C'           T02tla
050400021121     c                   call      'TIBS02R'
050500021121     c                   parm                    KPJBA
050600021121     c                   parm                    TIBS02DS
050700021121      *
050800021121      * Chiudo archivi aperti
050900021121     c                   close     FNDCT13L
051000021121     c                   if        $Stampa  = *on
051100021121     c                   close     FIDN74P
051200021121     c                   eval      $Stampa  = *off
051300021121     c                   endif
051400021121      *
051500021121      * Termino !
051600021121     c                   eval      *INLR    = *on
051700021121     c                   return
051800021121      *
051900021121     c                   ENDSR
052000021121
052100021121      ****************************************************************
052200021121      * RICHIAMO PGM RICERCA/CONTROLLO TABELLE
052300021121      ****************************************************************
052400021121     c     CallTIBS02    BEGSR
052500021121      *
052600021121     c                   movel     'C'           T02mod
052700021121     c                   movel     KNSIF         T02sif
052800021121      *
052900021121     c                   call      'TIBS02R'
053000021121     c                   parm                    KPJBA
053100021121     c                   parm                    TIBS02DS
053200021121      *
053300021121     c                   ENDSR
053400021121
053500021121      ****************************************************************
053600021121      * CONVERSIONE DATA DAL FORMATO AAAA/MM/GG AL FORMATO AA/GGG
053700021121      ****************************************************************
053800021121     c     CNVDAT        BEGSR
053900021121      *
054000021121     c                   clear                   WLBdat
054100021121     c                   eval      G02inv = WdataYMD
054200021121     c                   eval      G02err = '3'
054300021121     c                   call      'XSRDA8'
054400021121     c                   parm                    WLBDAT
054500021121      *
054600021121     c     *eur          move      G02dat        WdataJUL
054700021121      *
054800021121     c                   ENDSR
054900021121
055000021121      ****************************************************************
055100021121      * DEFINIZIONE KLIST
055200021121      ****************************************************************
055300021121     c     DefKLIST      BEGSR
055400021121      *
055500021121      * file TITAS30C
055600021121     c     K04DCT02      klist
055700021121     c                   kfld                    DCTaas
055800021121     c                   kfld                    DCTlnp
055900021121     c                   kfld                    DCTnrs
056000021121     c                   kfld                    DCTnsp
056100021121      *
056200021121      * file TITA430C
056300021121     c     K05TA430      klist
056400021121     c                   kfld                    DCTaas
056500021121     c                   kfld                    DCTlnp
056600021121     c                   kfld                    DCTnrs
056700021121     c                   kfld                    DCTnsp
056800021121     c                   kfld                    TA4trc
056900021121      * file FNDET01L
057000021121     c     K02DET01      klist
057100021121     c                   kfld                    DCTaae
057200021121     c                   kfld                    DCTnev
057300021121      * file FNDCS01L
057400021121     c     K07DCS01      klist
057500021121     c                   kfld                    DCStpd
057600021121     c                   kfld                    DCSnkt
057700021121     c                   kfld                    DCSstd
057800021121     c                   kfld                    DCSdim
057900021121     c                   kfld                    DCShim
058000021121     c                   kfld                    DCSnks
058100021121     c                   kfld                    DCStrc
058200021121      * file FNDCL01L
058300021121     c     K03DCL01      klist
058400021121     c                   kfld                    DCTaac
058500021121     c                   kfld                    DCTfil
058600021121     c                   kfld                    DCTnca
058700021121      *
058800021121     c                   ENDSR
