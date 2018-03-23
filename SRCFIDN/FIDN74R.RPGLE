000100120424     /*PRM  dbgview(*source)
000200120424     /*END
000300021121      ****************************************************************
000400021121      *    Elenco C.A. per rivalsa FEDEX   -   batch                 *
000500021121      ****************************************************************
000600021121
000700021121      ****************************************************************
000800021121      *  RIEPILOGO INDICATORI                                        *
000900021121      *--------------------------------------------------------------*
001000021121      * 01 - RISTAMPA                                                *
001100021121      * 02 - STAMPO Avviso Danno                                     *
001200021121      * 03 - STAMPO Evento                                           *
001300021121      * 04 - STAMPO Causale Chiusura                                 *
001400021121      * 41 - EOF FIDN74P                                             *
001500021121      ****************************************************************
001600021121
001700021121     H DECEDIT('0,') DATEDIT(*DMY.)
001800021121
001900021121     FFNDCT13L  UF   E           K DISK
002000021121     FTITA430C  IF   E           K DISK
002100021121     FTITAS30C  IF   E           K DISK
002200021121     FTNTMD01L  IF   E           K DISK
002300021121     FFNDCS01L  IF   E           K DISK
002400021121     FFNDET01L  IF   E           K DISK
002500021121     FFNDCL01L  IF   E           K DISK
002600120424      *
002700120424     fWFDFE00F  Uf A e           k disk    usropn
002800021121      *
002900021121     FFIDN74P   O    E             PRINTER oflind(*IN41) usropn
003000021121
003100021121      *------------- SCHIERE
003200021121      * - Note dal file FNDCS00F
003300021121     D $NoteA          s                   like(DCSnot) dim(3) inz
003400021121      *
003500021121      *------------- DS ESTERNE
003600021121      * - Parametri
003700021121     D KPJBA         e ds
003800021121     D FIDN73DS      e ds                  inz
003900050711      *
004000050711      * - Parametri x Controllo profilo utenti
004100050711     d TIBS34ds      e ds
004200050711      * - Ds di riferimento al file esterno AZUTE00F
004300050711     d AZUTEds       e ds                  extname(AZUTE00F)
004400050711      * - Ds per dati organigramma
004500050711     d dDATIUTE      e ds
004600021121      *
004700070927     D DSbl4E        e ds                  inz
004800021121      *
004900021121      * Flag operativi - campo DCTFLO del file FNDCT00F
005000021121     D DDCT01        e ds                  inz
005100021121      *
005200021121      * Tabelle
005300021121     D TIBS02DS      e ds                  inz
005400021121     D DTADdct       e ds                  extname(DTAD)     inz
005500021121     D                                     prefix(A)
005600021121     D DTADdet       e ds                  extname(DTAD)     inz
005700021121     D                                     prefix(E)
005800021121     D DCCH          e ds                  inz
005900021121      *
006000021121      * Ridefinizione tracciati record
006100021121     D DS_TITAS      e ds                  extname(TITAS30C) inz
006200021121     D DS_TNTMD      e ds                  extname(TNTMD00F) inz
006300021121     D DS_FNDCS      e ds                  extname(FNDCS00F) inz
006400021121     D DS_FNDCL      e ds                  extname(FNDCL00F) inz
006500021121      *
006600021121      *------------- DS DI STATO
006700021121     D STATUS         sds
006800021121     D  VTCPGM           *proc
006900021121      *
007000021121      *------------- DS INTERNE
007100021121      * - Controllo/Inversione date
007200021121     D WLBDAT          ds                  inz
007300021121     D  G02DAT                 1      8  0
007400021121     D  G02INV                 9     16  0
007500021121     D  G02ERR                17     17
007600021121     D  G02TGI                18     22  0
007700021121      * - Chiave tipo descrizione per file FNDCS01L
007800021121     D DSnumCA         ds                  inz
007900021121     D  DCTaac
008000021121     D  DCTfil
008100021121     D  DCTnca
008200021121      *
008300021121      *------------- VARIABILI
008400021121      * - data in formato AAAAMMGG
008500021121     D WdataYMD        s              8  0 inz
008600021121      * - data in formato GIULIANO
008700021121     D WdataJUL        s               d   inz datfmt(*JUL)
008800021121      * - data odierna in formato GIULIANO
008900021121     D WoggiJUL        s               d   inz datfmt(*JUL)
009000021121      * - campo di comodo per memorizzazione DCTPER
009100021121     D WdctPER         s                   inz like(DCTper)
009200120424      *
009300021121      * - indice schiera
009400021121     D XX              s              3  0 inz
009500120424      *
009600021121      * - flag record da elaborare
009700021121     D $OKrec          s              1    inz(*off)
009800021121      * - flag stampato almeno un record
009900021121     D $Stampa         s              1    inz(*off)
010000021121      *
010100021121      *------------- COSTANTI
010200021121      * - causale di chiusura C.A.
010300021121     D Wchca           c                   const(900)
010400120424
010500120424       //--------------------------------------------------------------
010600120424       //?Definizione prototipi procedure.                             ?
010700120424       //--------------------------------------------------------------
010800120424
010900120424       // -?Parametri API QCAPCMD (Process Commands)?
011000120424     d Qcmd            s           2048    inz  varying
011100120424      /copy qSysInc/qRpgleSrc,QCAPCMD
011200120424       // -?API QCAPCMD (Process Commands)?
011300120424      /copy gaitrasrc/srcProtoPR,QCAPCMD
011400120424
011500120424       // -?Parametri gestione errori API.?
011600120424      /copy qsysinc/qrpglesrc,QUSEC
011700021121
011800021121      ****************************************************************
011900021121      *                   C   I   C   L   O                          *
012000021121      ****************************************************************
012100021121
012200021121     c     *ENTRY        PLIST
012300021121     c                   PARM                    KPJBA
012400021121      *
012500021121      * operazioni iniziali
012600021121     c                   exsr      SR_INZSR
012700021121      *
012800021121      * mi posiziono con la data sul file testate C.A.
012900021121     c                   if        I73ris   = 'S'
013000021121     c                   eval      *in01    = *on
013100021121     c                   move      WdataJUL      WdctPER
013200021121     c                   else
013300021121     c                   eval      *in01    = *off
013400021121     c                   eval      WdctPER  = *all'9'
013500021121     c                   endif
013600021121     c     WdctPER       setll     FNDCT000
013700021121      *
013800021121      * ciclo di lettura del file principale
013900021121      *
014000021121do  1c                   do        *hival
014100021121      *
014200021121     c     WdctPER       reade     FNDCT000
014300021121      *
014400021121      * e.o.f.
014500021121if  2c                   if        %eof(FNDCT13L)
014600021121     c                   leave
014700021121e   2c                   endif
014800021121      *
014900021121      * selezione record
015000021121     c                   exsr      SR_SELEZ
015100021121if  2c                   if        $OKrec = *off
015200021121     c                   iter
015300021121e   2c                   endif
015400021121      *
015500021121      * recupero dei dati aggiuntivi per la stampa
015600021121     c                   exsr      SR_DATI
015700120424      *
015800120424      * scrittura WrkF
015900120424     c                   if        I73ele <> 'S'
016000120424     c                   exsr      SR_WRITE
016100120424     c                   endif
016200021121      *
016300021121      * stampa
016400120424     c                   if        I73ele <> 'F'
016500021121     c                   exsr      SR_STAMPA
016600120424     c                   endif
016700021121      *
016800021121e   1c                   enddo
016900021121      *
017000021121      * Fine
017100021121     c                   exsr      SR_ENDPGM
017200021121
017300021121      ****************************************************************
017400021121      * ROUTINE INIZIALE
017500021121      ****************************************************************
017600021121     c     SR_INZSR      BEGSR
017700021121      *
017800120424     c                   eval      *INLR    = *on
017900021121     c                   movel     KPJBU         FIDN73DS
018000021121      *
018100050711      * Reperisco nome azienda per stampa PREPAGATI
018200050711     c     *dtaara       define    §azute        AZUTEds
018300050711     c     *dtaara       define    §datiute      dDATIUTE
018400050711     c                   in(E)     *dtaara
018500050711     c                   if        %ERROR or RSUT = *blanks
018600050711     c                   clear                   Tibs34Ds
018700050711     c                   call      'TIBS34R'
018800050711     c                   parm                    Tibs34Ds
018900050711     c                   in        *dtaara
019000050711     c                   endif
019100021121      *
019200021121     c                   move      *date         WoggiJUL
019300050711      *
019400050711     c                   eval      *in41    = *on
019500021121      *
019600021121if  1c                   if        I73ris   = 'S'
019700021121     c                   eval      WdataYMD = I73dat
019800021121     c                   exsr      CnvDat
019900021121     c                   z-add     G02dat        PTdata
020000021121     c***                move      WdataJUL      WdctPER
020100021121x   1c                   else
020200021121     c                   move      *date         PTdata
020300021121e   1c                   endif
020400120424      *
020500120424if  1c                   If        I73ele <> 'S'
020600120424      *
020700120424     c                   eval      Qcmd = 'CLRPFM file('
020800120424if  2c                   if        %subst( KNSIF : 7 : 1 ) = 'P'
020900120424     c                   eval      Qcmd += 'GAITRAAZP'
021000120424x   2c                   else
021100120424     c                   eval      Qcmd += 'GAITRAAZM'
021200120424e   2c                   endif
021300120424     c                   eval      Qcmd += '/WFDFE00F)'
021400120424      *
021500120424     c                   exsr      sr_ExecCmd
021600120424      *
021700120424if  2c                   if        Qusei <> *blank
021800120424     c                   exsr      sr_PrintErr
021900120424     c                   return
022000120424e   2c                   endif
022100120424      *
022200120424     c                   open      WFDFE00F
022300120424      *
022400120424e   1c                   EndIf
022500021121      *
022600021121     c                   ENDSR
022700021121
022800021121      ****************************************************************
022900021121      * SELEZIONE DEI RECORD DA ELABORARE
023000021121      ****************************************************************
023100021121     c     SR_SELEZ      BEGSR
023200021121      *
023300021121     c                   reset                   $OKrec
023400021121      *
023500021121      * Leggo una v.l. che prevede già:
023600021121      * - la OMIT dei record con il campo DCTper = *zeros
023700021121      * Tale campo è impostato dagli appositi programmi SOLO per:
023800021121      * - spedizioni Porto Franco, export DPD
023900021121      *              (§DCTport = 'F', §DCTtisp = 'E' e §DCTeuro = 'D')
024000021121      * - spedizioni export FEDEX    (§DCTtisp = 'E' e §DCTeuro = 'F')
024100021121      * - comunque con responsabilità PARTNER           (DCTres = 'P')
024200021121      * - e con tipo gestione "Pratica Assicurativa"    (DCTfpr = 'P')
024300021121      *
024400021121     c                   movel     DCTflo        dDCT01
024500021121      *
024600021121      * record annullati
024700021121     c     DCTatb        cabne     *blanks       EndSelez
024800021121      * Network estero non FedEx
024900021121     c     §DCTeuro      cabne     'F'           EndSelez
025000021121      * Fase 900 (se NON RIstampa)
025100021121if  1c                   if            I73ris <> 'S'
025200021121     c                             and DCTfca <> WCHCA
025300021121     c                   goto      EndSelez
025400021121e   1c                   endif
025500021121      *
025600021121      * record da elaborare
025700021121     c                   eval      $OKrec = *on
025800021121      *
025900021121     c     EndSelez      ENDSR
026000021121
026100021121      ****************************************************************
026200021121      * RECUPERO DEI DATI AGGIUNTIVI PER LA STAMPA
026300021121      ****************************************************************
026400021121     c     SR_DATI       BEGSR
026500021121      *
026600021121      * Aggancio file bolle sede
026700021121     c     K04DCT02      chain     TITAS30C
026800021121if  1c                   if        not %found(TITAS30C)
026900021121     c                   clear                   DS_TITAS
027000021121e   1c                   endif
027100021121      *
027200021121      * Aggancio file riferimenti bolle
027300070927     c                   eval      TA4trc = 'E'
027400070927     c     K05TA430      chain     TITA430C
027500070927if  1c                   if        %found(TITA430C)
027600070927     c                   movel     TA4not        dsbl4e
027700070927x   1c                   else
027800070927     c                   clear                   dsbl4e
027900070927e   1c                   endif
028000021121      *
028100021121      * Aggancio file mittenti/destinatari
028200021121     c     K04DCT02      chain     TNTMD000
028300021121if  1c                   if        not %found(TNTMD01L)
028400021121     c                   clear                   DS_TNTMD
028500021121e   1c                   endif
028600021121      *
028700021121      * Aggancio testata evento danni
028800021121if  1c                   if        DCTnev > *zeros
028900021121     c                   eval      *in03  = *on
029000021121     c     K02DET01      chain     FNDET000
029100021121if  2c                   if        not %found(FNDET01L)
029200021121     c                             or  DETatb <> *blanks
029300021121     c                   clear                   DETtad
029400021121e   2c                   endif
029500021121x   1c                   else
029600021121     c                   eval      *in03  = *off
029700021121     c                   clear                   DETtad
029800021121e   1c                   endif
029900021121      *
030000021121      * Aggancio file descrizioni per descrizione anomalia
030100021121     c                   clear                   XX
030200021121     c                   clear                   $NoteA
030300021121     c                   clear                   DS_FNDCS
030400021121     c                   movel     'C'           DCStpd
030500021121     c                   movel(p)  DSnumCA       DCSnkt
030600021121     c                   movel     'A'           DCSstd
030700021121     c                   clear                   DCSdim
030800021121     c                   clear                   DCShim
030900021121     c                   clear                   DCSnks
031000021121     c                   movel     'D'           DCStrc
031100021121     c     K07DCS01      chain     FNDCS000
031200021121if  1c                   if        %found(FNDCS01L)
031300021121do  2c                   dou           %eof(FNDCS01L)
031400021121     c                             or  XX >= 3
031500021121if  3c                   if        DCSatb =  *blanks
031600021121     c                   add       1             XX
031700021121     c                   movel     DCSnot        $NoteA(XX)
031800021121e   3c                   endif
031900021121     c     K07DCS01      reade     FNDCS000
032000021121e   2c                   enddo
032100021121e   1c                   endif
032200021121      *
032300021121      * Aggancio file descrizioni per descrizione pezzi mancanti
032400021121     c                   clear                   DS_FNDCS
032500021121if  1c                   if        DCTnpz > *zeros
032600021121     c                   movel     'C'           DCStpd
032700021121     c                   movel(p)  DSnumCA       DCSnkt
032800021121     c                   movel     'P'           DCSstd
032900021121     c                   clear                   DCSdim
033000021121     c                   clear                   DCShim
033100021121     c                   clear                   DCSnks
033200021121     c                   movel     'D'           DCStrc
033300021121     c     K07DCS01      chain     FNDCS000
033400021121do  1c                   if        not %found(FNDCS01L)
033500021121     c                             or  DCSatb <> *blanks
033600021121     c                   clear                   DCSnot
033700021121e   1c                   endif
033800021121e   1c                   endif
033900021121      *
034000021121      * Aggancio file liquidazione C.A.
034100021121     c     K03DCL01      chain     FNDCL000
034200021121if  1c                   if        not %found(FNDCL01L)
034300021121     c                             or  DCLatb <> *blanks
034400021121     c                   clear                   DS_FNDCL
034500021121e   1c                   endif
034600021121      *
034700021121      * Decodifico tipo anomalia
034800021121     c                   clear                   TIBS02DS
034900021121     c                   movel     'TAD'         T02cod
035000021121     c                   movel(p)  DCTtad        T02ke1
035100021121     c                   exsr      CallTIBS02
035200021121     c                   movel     T02uni        DTADdct
035300021121      *
035400021121      * Decodifico evento
035500021121     c                   clear                   DTADdet
035600021121if  1c                   if        *in03
035700021121     c                   clear                   TIBS02DS
035800021121     c                   movel     'TAD'         T02cod
035900021121     c                   movel     DETtad        T02ke1
036000021121     c                   exsr      CallTIBS02
036100021121     c                   movel     T02uni        DTADdet
036200021121e   1c                   endif
036300021121      *
036400021121      * Decodifico eventuale causale di chiusura
036500021121     c                   clear                   DCCH
036600021121     c                   eval      *in04      =  *off
036700021121if  1c                   if        DCTcch   <> *blanks
036800021121     c                   eval      *in04      =  *on
036900021121     c                   clear                   TIBS02DS
037000021121     c                   movel     'CCH'         T02cod
037100021121     c                   movel(p)  DCTcch        T02ke1
037200021121     c                   exsr      CallTIBS02
037300021121     c                   movel     T02uni        DCCH
037400021121e   1c                   endif
037500021121      *
037600021121     c                   ENDSR
037700120424      *
037800120424      ****************************************************************
037900120424      * SCRIVO IL RECORD DI LAVORO
038000120424      ****************************************************************
038100120424     c     SR_WRITE      BEGSR
038200120424      *
038300120424      * IMPOSTAZIONE CAMPI DEL FILE
038400120424     c                   clear                   WFDFE000
038500120424      *
038600120424      * - Estrazione
038700120424     c                   eval      DFEute     =  KNmUs
038800120424     c                   eval      DFEdtec    =  *date
038900120424      *
039000120424      * - Comunicazione Anomalia
039100120424     c                   eval      DFEaac     =  DCTaac
039200120424     c                   eval      DFEfil     =  DCTfil
039300120424     c                   eval      DFEnca     =  DCTnca
039400120424      * - Avviso Presunto Danno
039500120424     c                   eval      DFEdit     =  DCTdit
039600120424     c                   eval      DFEprn     =  DCTprn
039700120424     c                   eval      DFEpra     =  DCTpra
039800120424      * - Evento
039900120424if  1c                   if        *in03
040000120424     c                   eval      DFEaae = DCTpra
040100120424     c                   eval      DFEnev = DCTnca
040200120424if  2c                   if        E§TADdest  <> *blanks
040300120424     c                   movel     E§TADdest     DFEtae
040400120424x   2c                   else
040500120424     c                   movel     E§TADdesc     DFEtae
040600120424e   2c                   endif
040700120424e   1c                   endif
040800120424      * - Numero AWB Parcel
040900120424     c                   eval      DFEnra     =  §B4erp
041000120424      * - Spedizione
041100120424     c                   eval      DFElnp     =  DCTlnp
041200120424     c                   eval      DFEnrs     =  DCTnrs
041300120424     c                   eval      DFEnsp     =  DCTnsp
041400120424     c                   z-add     TASmgs        WdataYMD
041500120424     c                   movel     DCTaas        WdataYMD
041600120424     c                   exsr      CNVDAT
041700120424     c                   z-add     G02dat        DFEdsp
041800120424      * - Mittente
041900120424     c                   movel     TMDrsm        DFErsm
042000120424     c                   movel     TMDinm        DFEinm
042100120424     c                   movel     TMDcam        DFEcam
042200120424     c                   movel     TMDlom        DFElom
042300120424     c                   movel     TMDprm        DFEprm
042400120424     c                   movel     TMDnzm        DFEnam
042500120424      * - Destinatario
042600120424     c                   movel     TMDrsd        DFErsd
042700120424     c                   movel     TMDind        DFEind
042800120424     c                   movel     TMDcad        DFEcad
042900120424     c                   movel     TMDlod        DFElod
043000120424     c                   movel     TMDprd        DFEprd
043100120424     c                   movel     TMDnzd        DFEnad
043200120424      *
043300120424      * - Colli in bolla
043400120424     c                   eval      DFEncl     =  TASncl
043500120424      * - Peso Kg in bolla
043600120424     c                   eval      DFEpkf     =  TASpkf
043700120424      *
043800120424      * - Colli danneggiati/mancanti
043900120424     c                   eval      DFEncn     =  DCTncn
044000120424      * - Peso Kg danneggiato/mancanti
044100120424     c                   eval      DFEpkd     =  DCTpkd
044200120424      * - Pezzi danneggiati/mancanti
044300120424     c                   eval      DFEnpz     =  DCTnpz
044400120424      * - Note
044500120424     c                   movel     DCSnot        DFEdpz
044600120424      *
044700120424      * - Anomalia
044800120424if  1c                   if        A§TADdest  <> *blanks
044900120424     c                   movel     A§TADdest     DFEtad
045000120424x   1c                   else
045100120424     c                   movel     A§TADdesc     DFEtad
045200120424e   1c                   endif
045300120424      * - Note aggiuntive (descrizione anomalia)
045400120424     c                   movel     $NoteA(1)     DFEdan
045500120424     c                   movel(p)  $NoteA(2)     DFEkos
045600120424     c                   move      $NoteA(3)     DFEkos
045700120424      *
045800120424      * - Importo rimborsato
045900120424if  1c                   if        DCLipl < DCLipt
046000120424     c                   eval      DFEipr = 'VEDI C.A.'
046100120424x   1c                   else
046200120424     c                   eval      DFEipr = %trim(%editc(DCLipl:'K'))
046300120424e   1c                   endif
046400120424      * - Causale di chiusura
046500120424if  1c                   if        DCTcch     <> *blanks
046600120424     c                   eval      DFEcch     =  DCTcch
046700120424     c                   eval      DFEccd     =  §CCHdesc
046800120424x   1c                   else
046900120424     c                   clear                   DFEcch
047000120424     c                   clear                   DFEccd
047100120424e   1c                   endif
047200120424      *
047300120424      * SCRITTURA DLE RECORD
047400120424     C                   WRITE     WFDFE000
047500120424      *
047600120424     c                   ENDSR
047700021121
047800021121      ****************************************************************
047900021121      * STAMPA
048000021121      ****************************************************************
048100021121     c     SR_STAMPA     BEGSR
048200021121      *
048300021121      * IMPOSTAZIONE CAMPI IN STAMPA
048400021121      *
048500021121      * Campi del rec. DN74PD1:
048600021121      * - Avviso Presunto Danno
048700021121     c                   if           DCTdit  <> *blanks
048800021121     c                             or DCTprn  <> *zeros
048900021121     c                             or DCTpra  <> *zeros
049000021121     c                   eval      *in02      =  *on
049100021121     c                   eval      PD1dit     =  DCTdit
049200021121     c                   eval      PD1prn     =  DCTprn
049300021121     c                   eval      PD1pra     =  DCTpra
049400021121     c                   else
049500021121     c                   eval      *in02      =  *off
049600021121     c                   endif
049700021121      * - Comunicazione Anomalia
049800021121     c                   eval      PD1fil     =  DCTfil
049900021121     c                   eval      PD1nca     =  DCTnca
050000021121     c                   eval      PD1aac     =  DCTaac
050100021121      * - Riferimento FedEx - Numero AWB
050200070927     c****               eval      PD1nrawb   =  §TA4arpt
050300070927     c                   movel     §b4erp        PD1nrawb
050400021121      * - Spedizione
050500021121     c                   eval      PD1lnp     =  DCTlnp
050600021121     c                   eval      PD1nrs     =  DCTnrs
050700021121     c                   eval      PD1nsp     =  DCTnsp
050800021121     c                   z-add     TASmgs        WdataYMD
050900021121     c                   movel     DCTaas        WdataYMD
051000021121     c                   exsr      CNVDAT
051100021121     c                   z-add     G02dat        PD1dsp
051200021121      * - Mittente
051300021121     c                   movel     TMDrsm        PD1rsm
051400021121     c                   movel     TMDinm        PD1inm
051500021121     c                   movel     TMDcam        PD1cam
051600021121     c                   movel     TMDlom        PD1lom
051700021121     c                   movel     TMDprm        PD1prm
051800021121     c                   movel     TMDnzm        PD1nzm
051900021121      * - Destinatario
052000021121     c                   movel     TMDrsd        PD1rsd
052100021121     c                   movel     TMDind        PD1ind
052200021121     c                   movel     TMDcad        PD1cad
052300021121     c                   movel     TMDlod        PD1lod
052400021121     c                   movel     TMDprd        PD1prd
052500021121     c                   movel     TMDnzd        PD1nzd
052600021121      *
052700021121      * Campi del rec. DN74PD2:
052800021121      * - Colli in bolla
052900021121     c                   eval      PD2ncl     =  TASncl
053000021121      * - Peso Kg in bolla
053100021121     c                   eval      PD2pkf     =  TASpkf
053200021121      * - Evento
053300021121if  1c                   if        *in03
053400021121     c                   eval      PD2kev     =  %editc(DCTpra:'Z') + '/' +
053500021121     c                                        %trim(%editc(DCTnca:'Z'))
053600021121if  2c                   if        E§TADdest  <> *blanks
053700021121     c                   movel     E§TADdest     PD2tae
053800021121x   2c                   else
053900021121     c                   movel     E§TADdesc     PD2tae
054000021121e   2c                   endif
054100021121e   1c                   endif
054200021121      *
054300021121      * Campi del rec. DN74PD3:
054400021121      * - Colli danneggiati/mancanti
054500021121     c                   eval      PD3ncn     =  DCTncn
054600021121      * - Peso Kg danneggiato/mancanti
054700021121     c                   eval      PD3pkd     =  DCTpkd
054800021121      * - Pezzi danneggiati/mancanti
054900021121     c                   eval      PD3npz     =  DCTnpz
055000021121      * - Note
055100021121     c                   movel     DCSnot        PD3dpz
055200021121      *
055300021121      * Campi del rec. DN74PD4:
055400021121      * - Anomalia
055500021121if  1c                   if        A§TADdest  <> *blanks
055600021121     c                   movel     A§TADdest     PD4tad
055700021121x   1c                   else
055800021121     c                   movel     A§TADdesc     PD4tad
055900021121e   1c                   endif
056000021121      * - Note aggiuntive (descrizione anomalia)
056100021121     c                   movel     $NoteA(1)     PD4dan
056200021121     c                   movel(p)  $NoteA(2)     PD4kos
056300021121     c                   move      $NoteA(3)     PD4kos
056400021121      *
056500021121      * Campi del rec. DN74PD5:
056600021121      * - Importo rimborsato
056700030409     c***                eval      PD5ipr = %trim(%editc(DCLipl+DCLipt:'K'))
056800030409     c***                                 + ' ' + DCLvpl
056900030409if  1c                   if        DCLipl < DCLipt
057000030409     c                   eval      PD5ipr = 'VEDI C.A.'
057100030409x   1c                   else
057200030409     c                   eval      PD5ipr = %trim(%editc(DCLipl:'K'))
057300030409     c                                    + ' ' + DCLvpl
057400030409e   1c                   endif
057500021121      * - Causale di chiusura
057600021121if  1c                   if        DCTcch     <> *blanks
057700021121     c                   eval      *in04      =  *on
057800021121     c                   eval      PD5cch     =  DCTcch
057900021121     c                   eval      PD5ccd     =  §CCHdesc
058000021121x   1c                   else
058100021121     c                   clear                   PD5cch
058200021121     c                   clear                   PD5ccd
058300021121e   1c                   endif
058400021121      *
058500021121      * STAMPA
058600021121      *
058700021121      * - apertura iniziale del prtf
058800021121if  2c                   if        $Stampa    =  *off
058900021121     c                   open      FIDN74P
059000021121e   2c                   endif
059100021121      * - testata
059200021121if  2c                   if        *IN41
059300021121     c                   write     DN74PT
059400021121     c                   eval      *IN41      =  *off
059500021121     c                   endif
059600021121      * - separatore
059700021121if  2c                   if        $Stampa    =  *on
059800021121     c                   write     DN74P1
059900021121     c                   endif
060000021121      * - dettaglio
060100021121     c                   write     DN74PD1
060200021121     c                   write     DN74PD2
060300021121     c                   write     DN74PD3
060400021121     c                   write     DN74PD4
060500021121     c                   write     DN74PD5
060600021121     c                   eval      $Stampa    =  *on
060700021121      *
060800021121      * AGGIORNAMENTO DEL RECORD IN ESAME DEL FILE FNDCT13L
060900021121      *   (se NON ristampa)
061000021121      *
061100021121     c                   if        I73ris     =  *blanks
061200021121     c                   move      WoggiJUL      DCTper
061300021121     c                   update    FNDCT000
061400021121     c                   endif
061500021121      *
061600021121     c                   ENDSR
061700021121
061800021121      ****************************************************************
061900021121      * ROUTINE FINALE
062000021121      ****************************************************************
062100021121     c     SR_ENDPGM     BEGSR
062200021121      *
062300021121      * Segnalo la mancanza di dati da stampare
062400021121      * - Pratiche Assicurative NON in Franchigia (per Consuldanni)
062500120424if  1c                   if             $Stampa  = *off
062600120424     c                             and  I73ele  <> 'F'
062700021121     c                   open      FIDN74P
062800021121     c                   eval      *in09    = *off
062900021121     c                   write     DN74PT
063000021121     c                   write     DN74PD0
063100021121     c                   eval      $Stampa  = *on
063200021121e   1c                   endif
063300021121      *
063400021121      * Stampo "Fine Stampa"
063500021121if  1c                   if        $Stampa  = *on
063600021121     c                   write     DN74PE
063700021121e   1c                   endif
063800021121      *
063900021121      * Chiudo TIBS02R
064000021121     c                   clear                   TIBS02DS
064100021121     c                   movel     'C'           T02tla
064200021121     c                   call      'TIBS02R'
064300021121     c                   parm                    KPJBA
064400021121     c                   parm                    TIBS02DS
064500021121      *
064600021121      * Chiudo archivi aperti
064700021121     c                   close     FNDCT13L
064800120424     c                   if        %open(WFDFE00F)
064900120424     c                   close     WFDFE00F
065000120424     c                   endif
065100021121     c                   if        $Stampa  = *on
065200021121     c                   close     FIDN74P
065300021121     c                   eval      $Stampa  = *off
065400021121     c                   endif
065500021121      *
065600021121      * Termino !
065700021121     c                   eval      *INLR    = *on
065800021121     c                   return
065900021121      *
066000021121     c                   ENDSR
066100021121
066200021121      ****************************************************************
066300021121      * RICHIAMO PGM RICERCA/CONTROLLO TABELLE
066400021121      ****************************************************************
066500021121     c     CallTIBS02    BEGSR
066600021121      *
066700021121     c                   movel     'C'           T02mod
066800021121     c                   movel     KNSIF         T02sif
066900021121      *
067000021121     c                   call      'TIBS02R'
067100021121     c                   parm                    KPJBA
067200021121     c                   parm                    TIBS02DS
067300021121      *
067400021121     c                   ENDSR
067500021121
067600021121      ****************************************************************
067700021121      * CONVERSIONE DATA DAL FORMATO AAAA/MM/GG AL FORMATO AA/GGG
067800021121      ****************************************************************
067900021121     c     CNVDAT        BEGSR
068000021121      *
068100021121     c                   clear                   WLBdat
068200021121     c                   eval      G02inv = WdataYMD
068300021121     c                   eval      G02err = '3'
068400021121     c                   call      'XSRDA8'
068500021121     c                   parm                    WLBDAT
068600021121      *
068700021121     c     *eur          move      G02dat        WdataJUL
068800021121      *
068900021121     c                   ENDSR
069000021121
069100021121      ****************************************************************
069200021121      * DEFINIZIONE KLIST
069300021121      ****************************************************************
069400021121     c     DefKLIST      BEGSR
069500021121      *
069600021121      * file TITAS30C
069700021121     c     K04DCT02      klist
069800021121     c                   kfld                    DCTaas
069900021121     c                   kfld                    DCTlnp
070000021121     c                   kfld                    DCTnrs
070100021121     c                   kfld                    DCTnsp
070200021121      *
070300021121      * file TITA430C
070400021121     c     K05TA430      klist
070500021121     c                   kfld                    DCTaas
070600021121     c                   kfld                    DCTlnp
070700021121     c                   kfld                    DCTnrs
070800021121     c                   kfld                    DCTnsp
070900021121     c                   kfld                    TA4trc
071000021121      * file FNDET01L
071100021121     c     K02DET01      klist
071200021121     c                   kfld                    DCTaae
071300021121     c                   kfld                    DCTnev
071400021121      * file FNDCS01L
071500021121     c     K07DCS01      klist
071600021121     c                   kfld                    DCStpd
071700021121     c                   kfld                    DCSnkt
071800021121     c                   kfld                    DCSstd
071900021121     c                   kfld                    DCSdim
072000021121     c                   kfld                    DCShim
072100021121     c                   kfld                    DCSnks
072200021121     c                   kfld                    DCStrc
072300021121      * file FNDCL01L
072400021121     c     K03DCL01      klist
072500021121     c                   kfld                    DCTaac
072600021121     c                   kfld                    DCTfil
072700021121     c                   kfld                    DCTnca
072800021121      *
072900021121     c                   ENDSR
073000120424      /free
073100120424       //--------------------------------------------------------------
073200120424       //?Esecuzione del comando (già impostato).                      ?
073300120424       //--------------------------------------------------------------
073400120424       BEGSR  sr_ExecCmd;
073500120424
073600120424         clear Qcap0100;
073700120424         Qcabcsdh = *off;
073800120424         Qcapa    = *off;
073900120424         Qcacmdss = *off;
074000120424         Qcaerved = *allX'00';
074100120424
074200120424         clear Qusec;
074300120424         Qusbprv  = %size(Qusec);
074400120424
074500120424         ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
074600120424                           %size(Qcap0100) : 'CPOP0100' : *omit :
074700120424                           0 : 0 : Qusec);
074800120424
074900120424         //if  Qusei <> *blank;
075000120424         //  ...;
075100120424         //endif;
075200120424
075300120424       ENDSR;
075400120424
075500120424       //--------------------------------------------------------------
075600120424       //?Stampa segnalazione dell'errore rilevato.                    ?
075700120424       //--------------------------------------------------------------
075800120424       BEGSR  sr_PrintErr;
075900120424
076000120424         // -?Stampa del Dump?
076100120424         Dump(A);
076200120424
076300120424         // -?Stampa del Job-Log?
076400120424         Qcmd = 'DSPJOBLOG job(*) output(*print)';
076500120424         exsr  sr_ExecCmd;
076600120424
076700120424       ENDSR;
076800120424      /end-free
