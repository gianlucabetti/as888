000100120424     /*PRM  dbgview(*source)
000200120424     /*END
000300020424      ****************************************************************
000400020424      *    Elenco C.A. per rivalsa DPD   -   batch                   *
000500020424      ****************************************************************
000600020424
000700020424      ****************************************************************
000800020424      *  RIEPILOGO INDICATORI                                        *
000900020424      *--------------------------------------------------------------*
001000020424      * 01 - RISTAMPA                                                *
001100020424      * 02 - STAMPO Avviso Danno                                     *
001200020424      * 03 - STAMPO Evento                                           *
001300020620      * 04 - STAMPO Causale Chiusura                                 *
001400020424      * 41 - EOF FIDN46P                                             *
001500020424      ****************************************************************
001600020424
001700020424     H DECEDIT('0,') DATEDIT(*DMY.)
001800020424
001900020424     FFNDCT13L  UF   E           K DISK
002000020424     FTITA430C  IF   E           K DISK
002100020424     FTITAS30C  IF   E           K DISK
002200020424     FTNTMD01L  IF   E           K DISK
002300020424     FFNDCS01L  IF   E           K DISK
002400020424     FFNDET01L  IF   E           K DISK
002500020424     FFNDCL01L  IF   E           K DISK
002600120424      *
002700120424     fWFDPD00F  Uf A e           k disk    usropn
002800020424      *
002900020424     FFIDN64P   O    E             PRINTER oflind(*IN41) usropn
003000020424
003100020424      *------------- SCHIERE
003200020424      * - Note dal file FNDCS00F
003300020424     D $NoteA          s                   like(DCSnot) dim(3) inz
003400020424      *
003500020424      *------------- DS ESTERNE
003600020424      * - Parametri
003700020424     D KPJBA         e ds
003800020424     D FIDN63DS      e ds                  inz
003900050711      *
004000050711      * - Parametri x Controllo profilo utenti
004100050711     d TIBS34ds      e ds
004200050711      * - Ds di riferimento al file esterno AZUTE00F
004300050711     d AZUTEds       e ds                  extname(AZUTE00F)
004400050711      * - Ds per dati organigramma
004500050711     d dDATIUTE      e ds
004600020424      *
004700060613     D DSBL4I        e ds                  inz
004800020424      *
004900020424      * Flag operativi - campo DCTFLO del file FNDCT00F
005000020424     D DDCT01        e ds                  inz
005100020424      *
005200020424      * Tabelle
005300020424     D TIBS02DS      e ds                  inz
005400020424     D DTADdct       e ds                  extname(DTAD)     inz
005500020424     D                                     prefix(A)
005600020424     D DTADdet       e ds                  extname(DTAD)     inz
005700020424     D                                     prefix(E)
005800020424     D DCCH          e ds                  inz
005900020424      *
006000020424      * Ridefinizione tracciati record
006100020424     D DS_TITAS      e ds                  extname(TITAS30C) inz
006200020424     D DS_TNTMD      e ds                  extname(TNTMD00F) inz
006300020424     D DS_FNDCS      e ds                  extname(FNDCS00F) inz
006400020424     D DS_FNDCL      e ds                  extname(FNDCL00F) inz
006500150212      *
006600150212      * -?Reperimento cod.cliente di un depot DPD?
006700150212     d TRULDEPds     e ds                  inz
006800020424      *
006900020424      *------------- DS DI STATO
007000020424     D STATUS         sds
007100020424     D  VTCPGM           *proc
007200020424      *
007300020424      *------------- DS INTERNE
007400020424      * - Controllo/Inversione date
007500020424     D WLBDAT          ds                  inz
007600020424     D  G02DAT                 1      8  0
007700020424     D  G02INV                 9     16  0
007800020424     D  G02ERR                17     17
007900020424     D  G02TGI                18     22  0
008000020424      * - Chiave tipo descrizione per file FNDCS01L
008100020424     D DSnumCA         ds                  inz
008200020424     D  DCTaac
008300020424     D  DCTfil
008400020424     D  DCTnca
008500020424      *
008600020424      *------------- VARIABILI
008700020424      * - data in formato AAAAMMGG
008800020424     D WdataYMD        s              8  0 inz
008900020424      * - data in formato GIULIANO
009000020424     D WdataJUL        s               d   inz datfmt(*JUL)
009100020424      * - data odierna in formato GIULIANO
009200020424     D WoggiJUL        s               d   inz datfmt(*JUL)
009300020424      * - campo di comodo per memorizzazione DCTPER
009400020424     D WdctPER         s                   inz like(DCTper)
009500120424      *
009600020424      * - indice schiera
009700020424     D XX              s              3  0 inz
009800120424      *
009900020424      * - flag record da elaborare
010000020424     D $OKrec          s              1    inz(*off)
010100020424      * - flag stampato almeno un record
010200020424     D $Stampa         s              1    inz(*off)
010300020424      *
010400020424      *------------- COSTANTI
010500020619      * - causale di chiusura C.A.
010600020619     D Wchca           c                   const(900)
010700120424
010800120424       //--------------------------------------------------------------
010900120424       //?Definizione prototipi procedure.                             ?
011000120424       //--------------------------------------------------------------
011100120424
011200120424       // -?Parametri API QCAPCMD (Process Commands)?
011300120424     d Qcmd            s           2048    inz  varying
011400120424      /copy qSysInc/qRpgleSrc,QCAPCMD
011500120424       // -?API QCAPCMD (Process Commands)?
011600120424      /copy gaitrasrc/srcProtoPR,QCAPCMD
011700120424
011800120424       // -?Parametri gestione errori API.?
011900120424      /copy qsysinc/qrpglesrc,QUSEC
012000020424
012100020424      ****************************************************************
012200020424      *                   C   I   C   L   O                          *
012300020424      ****************************************************************
012400020424
012500020424     c     *ENTRY        PLIST
012600020424     c                   PARM                    KPJBA
012700020424      *
012800020424      * operazioni iniziali
012900020424     c                   exsr      SR_INZSR
013000020424      *
013100020424      * mi posiziono con la data sul file testate C.A.
013200020424     c                   if        I63ris   = 'S'
013300020424     c                   eval      *in01    = *on
013400020424     c                   move      WdataJUL      WdctPER
013500020424     c                   else
013600020424     c                   eval      *in01    = *off
013700020424     c                   eval      WdctPER  = *all'9'
013800020424     c                   endif
013900020424     c     WdctPER       setll     FNDCT000
014000020424      *
014100020424      * ciclo di lettura del file principale
014200020424      *
014300020424do  1c                   do        *hival
014400020424      *
014500020424     c     WdctPER       reade     FNDCT000
014600020424      *
014700020424      * e.o.f.
014800020424if  2c                   if        %eof(FNDCT13L)
014900020424     c                   leave
015000020424e   2c                   endif
015100020424      *
015200020424      * selezione record
015300020424     c                   exsr      SR_SELEZ
015400020424if  2c                   if        $OKrec = *off
015500020424     c                   iter
015600020424e   2c                   endif
015700020424      *
015800020424      * recupero dei dati aggiuntivi per la stampa
015900020424     c                   exsr      SR_DATI
016000120424      *
016100120424      * scrittura WrkF
016200120424     c                   if        I63ele <> 'S'
016300120424     c                   exsr      SR_WRITE
016400120424     c                   endif
016500020424      *
016600020424      * stampa
016700120424     c                   if        I63ele <> 'F'
016800020424     c                   exsr      SR_STAMPA
016900120424     c                   endif
017000020424      *
017100020424e   1c                   enddo
017200020424      *
017300020424      * Fine
017400020424     c                   exsr      SR_ENDPGM
017500020424
017600020424      ****************************************************************
017700020424      * ROUTINE INIZIALE
017800020424      ****************************************************************
017900020424     c     SR_INZSR      BEGSR
018000020424      *
018100120424     c                   eval      *INLR    = *on
018200020424     c                   movel     KPJBU         FIDN63DS
018300020424      *
018400050711      * Reperisco nome azienda per stampa PREPAGATI
018500050711     c     *dtaara       define    §azute        AZUTEds
018600050711     c     *dtaara       define    §datiute      dDATIUTE
018700050711     c                   in(E)     *dtaara
018800050711     c                   if        %ERROR or RSUT = *blanks
018900050711     c                   clear                   Tibs34Ds
019000050711     c                   call      'TIBS34R'
019100050711     c                   parm                    Tibs34Ds
019200050711     c                   in        *dtaara
019300050711     c                   endif
019400020424      *
019500020424     c                   move      *date         WoggiJUL
019600020424      *
019700050711     c                   eval      *in41    = *on
019800020424      *
019900020424if  1c                   if        I63ris   = 'S'
020000020424     c                   eval      WdataYMD = I63dat
020100020424     c                   exsr      CnvDat
020200020424     c                   z-add     G02dat        PTdata
020300020424     c***                move      WdataJUL      WdctPER
020400020424x   1c                   else
020500020424     c                   move      *date         PTdata
020600020424e   1c                   endif
020700120424      *
020800120424if  1c                   If        I63ele <> 'S'
020900120424      *
021000120424     c                   eval      Qcmd = 'CLRPFM file('
021100120424if  2c                   if        %subst( KNSIF : 7 : 1 ) = 'P'
021200120424     c                   eval      Qcmd += 'GAITRAAZP'
021300120424x   2c                   else
021400120424     c                   eval      Qcmd += 'GAITRAAZM'
021500120424e   2c                   endif
021600120424     c                   eval      Qcmd += '/WFDPD00F)'
021700120424      *
021800120424     c                   exsr      sr_ExecCmd
021900120424      *
022000120424if  2c                   if        Qusei <> *blank
022100120424     c                   exsr      sr_PrintErr
022200120424     c                   return
022300120424e   2c                   endif
022400120424      *
022500120424     c                   open      WFDPD00F
022600120424      *
022700120424e   1c                   EndIf
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
024400021120      * - comunque con responsabilità PARTNER           (DCTres = 'P')
024500021120      * - e con tipo gestione "Pratica Assicurativa"    (DCTfpr = 'P')
024600020424      *
024700020424     c                   movel     DCTflo        dDCT01
024800020424      *
024900020424      * record annullati
025000020424     c     DCTatb        cabne     *blanks       EndSelez
025100020424      * Network estero non DPD
025200021120     c     §DCTeuro      cabne     'D'           EndSelez
025300020619      * Fase 900 (se NON RIstampa)
025400020619if  1c                   if            I63ris <> 'S'
025500020619     c                             and DCTfca <> WCHCA
025600020619     c                   goto      EndSelez
025700020619e   1c                   endif
025800020424      *
025900020424      * record da elaborare
026000020424     c                   eval      $OKrec = *on
026100020424      *
026200020424     c     EndSelez      ENDSR
026300020424
026400020424      ****************************************************************
026500020424      * RECUPERO DEI DATI AGGIUNTIVI PER LA STAMPA
026600020424      ****************************************************************
026700020424     c     SR_DATI       BEGSR
026800020424      *
026900020424      * Aggancio file bolle sede
027000020424     c     K04DCT02      chain     TITAS30C
027100020424if  1c                   if        not %found(TITAS30C)
027200020424     c                   clear                   DS_TITAS
027300020424e   1c                   endif
027400020424      *
027500020424      * Aggancio file riferimenti bolle
027600060613     c                   eval      TA4trc = 'I'
027700020424     c     K05TA430      chain     TITA430C
027800020424if  1c                   if        %found(TITA430C)
027900060613     c                   movel     TA4not        DSBL4I
028000020424x   1c                   else
028100060613     c                   clear                   DSBL4I
028200020424e   1c                   endif
028300020424      *
028400020424      * Aggancio file mittenti/destinatari
028500020424     c     K04DCT02      chain     TNTMD000
028600020424if  1c                   if        not %found(TNTMD01L)
028700020424     c                   clear                   DS_TNTMD
028800020424e   1c                   endif
028900150212      * -?Reperimento codice cliente DPD?
029000150212      *  ?Con il depot richiamo pgm per recuperare il "vero" cod.cliente?
029100150212     c                   clear                   TRULDEPds
029200150212     c                   eval      iDEPdpc = §B4ida
029300150212     c                   eval      iDEPdrf = ( DCTaas * 10000 ) + TASmgs
029400150212     c                   call      'TRULDEPR'
029500150212     c                   parm                    TRULDEPds
029600150212     c                   if        oDEPerr = *on
029700150212     c                   clear                   oDEPksc
029800150212     c                   endif
029900020424      *
030000020424      * Aggancio testata evento danni
030100020424if  1c                   if        DCTnev > *zeros
030200020424     c                   eval      *in03  = *on
030300020424     c     K02DET01      chain     FNDET000
030400020424if  2c                   if        not %found(FNDET01L)
030500020424     c                             or  DETatb <> *blanks
030600020424     c                   clear                   DETtad
030700020424e   2c                   endif
030800020424x   1c                   else
030900020424     c                   eval      *in03  = *off
031000020424     c                   clear                   DETtad
031100020424e   1c                   endif
031200020424      *
031300020424      * Aggancio file descrizioni per descrizione anomalia
031400020424     c                   clear                   XX
031500020424     c                   clear                   $NoteA
031600020424     c                   clear                   DS_FNDCS
031700020424     c                   movel     'C'           DCStpd
031800020424     c                   movel(p)  DSnumCA       DCSnkt
031900020424     c                   movel     'A'           DCSstd
032000020424     c                   clear                   DCSdim
032100020424     c                   clear                   DCShim
032200020424     c                   clear                   DCSnks
032300020424     c                   movel     'D'           DCStrc
032400020424     c     K07DCS01      chain     FNDCS000
032500020424if  1c                   if        %found(FNDCS01L)
032600020424do  2c                   dou           %eof(FNDCS01L)
032700020424     c                             or  XX >= 3
032800020424if  3c                   if        DCSatb =  *blanks
032900020424     c                   add       1             XX
033000020424     c                   movel     DCSnot        $NoteA(XX)
033100020424e   3c                   endif
033200020424     c     K07DCS01      reade     FNDCS000
033300020424e   2c                   enddo
033400020424e   1c                   endif
033500020424      *
033600020424      * Aggancio file descrizioni per descrizione pezzi mancanti
033700020424     c                   clear                   DS_FNDCS
033800020424if  1c                   if        DCTnpz > *zeros
033900020424     c                   movel     'C'           DCStpd
034000020424     c                   movel(p)  DSnumCA       DCSnkt
034100020424     c                   movel     'P'           DCSstd
034200020424     c                   clear                   DCSdim
034300020424     c                   clear                   DCShim
034400020424     c                   clear                   DCSnks
034500020424     c                   movel     'D'           DCStrc
034600020424     c     K07DCS01      chain     FNDCS000
034700020424do  1c                   if        not %found(FNDCS01L)
034800020424     c                             or  DCSatb <> *blanks
034900020424     c                   clear                   DCSnot
035000020424e   1c                   endif
035100020424e   1c                   endif
035200020424      *
035300020424      * Aggancio file liquidazione C.A.
035400020424     c     K03DCL01      chain     FNDCL000
035500020424if  1c                   if        not %found(FNDCL01L)
035600020424     c                             or  DCLatb <> *blanks
035700020424     c                   clear                   DS_FNDCL
035800020424e   1c                   endif
035900020424      *
036000020424      * Decodifico tipo anomalia
036100020424     c                   clear                   TIBS02DS
036200020424     c                   movel     'TAD'         T02cod
036300020424     c                   movel(p)  DCTtad        T02ke1
036400020424     c                   exsr      CallTIBS02
036500020424     c                   movel     T02uni        DTADdct
036600020424      *
036700020424      * Decodifico evento
036800020424     c                   clear                   DTADdet
036900020424if  1c                   if        *in03
037000020424     c                   clear                   TIBS02DS
037100020424     c                   movel     'TAD'         T02cod
037200020424     c                   movel     DETtad        T02ke1
037300020424     c                   exsr      CallTIBS02
037400020424     c                   movel     T02uni        DTADdet
037500020424e   1c                   endif
037600020424      *
037700020424      * Decodifico eventuale causale di chiusura
037800020424     c                   clear                   DCCH
037900020620     c                   eval      *in04      =  *off
038000020620if  1c                   if        DCTcch   <> *blanks
038100020620     c                   eval      *in04      =  *on
038200020424     c                   clear                   TIBS02DS
038300020424     c                   movel     'CCH'         T02cod
038400020424     c                   movel(p)  DCTcch        T02ke1
038500020424     c                   exsr      CallTIBS02
038600020424     c                   movel     T02uni        DCCH
038700020424e   1c                   endif
038800020424      *
038900020424     c                   ENDSR
039000120424      *
039100120424      ****************************************************************
039200120424      * SCRIVO IL RECORD DI LAVORO
039300120424      ****************************************************************
039400120424     c     SR_WRITE      BEGSR
039500120424      *
039600120424      * IMPOSTAZIONE CAMPI DEL FILE
039700120424     c                   clear                   WFDPD000
039800120424      *
039900120424      * - Estrazione
040000120424     c                   eval      DPDute     =  KNmUs
040100120424     c                   eval      DPDdtec    =  *date
040200120424      *
040300120424      * - Comunicazione Anomalia
040400120424     c                   eval      DPDaac     =  DCTaac
040500120424     c                   eval      DPDfil     =  DCTfil
040600120424     c                   eval      DPDnca     =  DCTnca
040700120424      * - Avviso Presunto Danno
040800120424     c                   eval      DPDdit     =  DCTdit
040900120424     c                   eval      DPDprn     =  DCTprn
041000120424     c                   eval      DPDpra     =  DCTpra
041100120424      * - Evento
041200120424if  1c                   if        *in03
041300120424     c                   eval      DPDaae = DCTpra
041400120424     c                   eval      DPDnev = DCTnca
041500120424if  2c                   if        E§TADdest  <> *blanks
041600120424     c                   movel     E§TADdest     DPDtae
041700120424x   2c                   else
041800120424     c                   movel     E§TADdesc     DPDtae
041900120424e   2c                   endif
042000120424e   1c                   endif
042100150212      * -?Codice cliente DPD?
042200150212     c                   eval      DPDksc     =  oDEPksc
042300120424      * - Codice DEPOT
042400120424     c                   eval      DPDida     =  §B4ida
042500120424      * - Numero PARCEL
042600120424     c                   eval      DPDipn     =  §B4ipn
042700120424      *   se lungo 11 aggiungo chk digits
042800120424     c                   if        %subst(§B4ipn:12:1) = ' '
042900120424     c                   eval      %subst(DPDipn:12:1) = §B4icd
043000120424     c                   endif
043100120424      * - Spedizione
043200120424     c                   eval      DPDlnp     =  DCTlnp
043300120424     c                   eval      DPDnrs     =  DCTnrs
043400120424     c                   eval      DPDnsp     =  DCTnsp
043500120424     c                   z-add     TASmgs        WdataYMD
043600120424     c                   movel     DCTaas        WdataYMD
043700120424     c                   exsr      CNVDAT
043800120424     c                   z-add     G02dat        DPDdsp
043900120424      * - Mittente
044000120424     c                   movel     TMDrsm        DPDrsm
044100120424     c                   movel     TMDinm        DPDinm
044200120424     c                   movel     TMDcam        DPDcam
044300120424     c                   movel     TMDlom        DPDlom
044400120424     c                   movel     TMDprm        DPDprm
044500120424     c                   movel     TMDnzm        DPDnam
044600120424      * - Destinatario
044700120424     c                   movel     TMDrsd        DPDrsd
044800120424     c                   movel     TMDind        DPDind
044900120424     c                   movel     TMDcad        DPDcad
045000120424     c                   movel     TMDlod        DPDlod
045100120424     c                   movel     TMDprd        DPDprd
045200120424     c                   movel     TMDnzd        DPDnad
045300120424      *
045400120424      * - Colli in bolla
045500120424     c                   eval      DPDncl     =  TASncl
045600120424      * - Peso Kg in bolla
045700120424     c                   eval      DPDpkf     =  TASpkf
045800120424      *
045900120424      * - Colli danneggiati/mancanti
046000120424     c                   eval      DPDncn     =  DCTncn
046100120424      * - Peso Kg danneggiato/mancanti
046200120424     c                   eval      DPDpkd     =  DCTpkd
046300120424      * - Pezzi danneggiati/mancanti
046400120424     c                   eval      DPDnpz     =  DCTnpz
046500120424      * - Note
046600120424     c                   movel     DCSnot        DPDdpz
046700120424      *
046800120424      * - Anomalia
046900120424if  1c                   if        A§TADdest  <> *blanks
047000120424     c                   movel     A§TADdest     DPDtad
047100120424x   1c                   else
047200120424     c                   movel     A§TADdesc     DPDtad
047300120424e   1c                   endif
047400120424      * - Note aggiuntive (descrizione anomalia)
047500120424     c                   movel     $NoteA(1)     DPDdan
047600120424     c                   movel(p)  $NoteA(2)     DPDkos
047700120424     c                   move      $NoteA(3)     DPDkos
047800120424      *
047900120424      * - Importo rimborsato
048000120424if  1c                   if        DCLipl < DCLipt
048100120424     c                   eval      DPDipr = 'VEDI C.A.'
048200120424x   1c                   else
048300120424     c                   eval      DPDipr = %trim(%editc(DCLipl:'K'))
048400120424e   1c                   endif
048500120424      * - Causale di chiusura
048600120424if  1c                   if        DCTcch     <> *blanks
048700120424     c                   eval      DPDcch     =  DCTcch
048800120424     c                   eval      DPDccd     =  §CCHdesc
048900120424x   1c                   else
049000120424     c                   clear                   DPDcch
049100120424     c                   clear                   DPDccd
049200120424e   1c                   endif
049300120424      *
049400120424      * SCRITTURA DLE RECORD
049500120424     C                   WRITE     WFDPD000
049600120424      *
049700120424     c                   ENDSR
049800020424
049900020424      ****************************************************************
050000020424      * STAMPA
050100020424      ****************************************************************
050200020424     c     SR_STAMPA     BEGSR
050300020424      *
050400020424      * IMPOSTAZIONE CAMPI IN STAMPA
050500020424      *
050600050711      * Campi del rec. DN64PD1:
050700020424      * - Avviso Presunto Danno
050800020424     c                   if           DCTdit  <> *blanks
050900020424     c                             or DCTprn  <> *zeros
051000020424     c                             or DCTpra  <> *zeros
051100020424     c                   eval      *in02      =  *on
051200020424     c                   eval      PD1dit     =  DCTdit
051300020424     c                   eval      PD1prn     =  DCTprn
051400020424     c                   eval      PD1pra     =  DCTpra
051500020424     c                   else
051600020424     c                   eval      *in02      =  *off
051700020424     c                   endif
051800020424      * - Comunicazione Anomalia
051900020424     c                   eval      PD1fil     =  DCTfil
052000020424     c                   eval      PD1nca     =  DCTnca
052100020424     c                   eval      PD1aac     =  DCTaac
052200150212      * -?Codice cliente DPD?
052300150212     c                   eval      PD1ksc     =  oDEPksc
052400020424      * - Codice DEPOT
052500060613     c                   eval      PD1depot   =  §B4ida
052600020424      * - Numero PARCEL
052700060613     c                   eval      PD1parcel  =  §B4ipn
052800060619     c* se lungo 11 aggiungo chk digits
052900060619     c                   if        %subst(§b4ipn:12:1)=' '
053000060619     c                   eval      %subst(pd1parcel:12:1)=§b4icd
053100060619     c                   endif
053200020424      * - Spedizione
053300020424     c                   eval      PD1lnp     =  DCTlnp
053400020424     c                   eval      PD1nrs     =  DCTnrs
053500020424     c                   eval      PD1nsp     =  DCTnsp
053600020424     c                   z-add     TASmgs        WdataYMD
053700020424     c                   movel     DCTaas        WdataYMD
053800020424     c                   exsr      CNVDAT
053900020424     c                   z-add     G02dat        PD1dsp
054000020424      * - Mittente
054100020424     c                   movel     TMDrsm        PD1rsm
054200020424     c                   movel     TMDinm        PD1inm
054300020424     c                   movel     TMDcam        PD1cam
054400020424     c                   movel     TMDlom        PD1lom
054500020424     c                   movel     TMDprm        PD1prm
054600020424     c                   movel     TMDnzm        PD1nzm
054700020424      * - Destinatario
054800020424     c                   movel     TMDrsd        PD1rsd
054900020424     c                   movel     TMDind        PD1ind
055000020424     c                   movel     TMDcad        PD1cad
055100020424     c                   movel     TMDlod        PD1lod
055200020424     c                   movel     TMDprd        PD1prd
055300020424     c                   movel     TMDnzd        PD1nzd
055400020424      *
055500020424      * Campi del rec. DN64PD2:
055600020424      * - Colli in bolla
055700020424     c                   eval      PD2ncl     =  TASncl
055800020424      * - Peso Kg in bolla
055900020424     c                   eval      PD2pkf     =  TASpkf
056000020424      * - Evento
056100020424if  1c                   if        *in03
056200020424     c                   eval      PD2kev     =  %editc(DCTpra:'Z') + '/' +
056300020424     c                                        %trim(%editc(DCTnca:'Z'))
056400020424if  2c                   if        E§TADdest  <> *blanks
056500020424     c                   movel     E§TADdest     PD2tae
056600020424x   2c                   else
056700020424     c                   movel     E§TADdesc     PD2tae
056800020424e   2c                   endif
056900020424e   1c                   endif
057000020424      *
057100020424      * Campi del rec. DN64PD3:
057200020424      * - Colli danneggiati/mancanti
057300020424     c                   eval      PD3ncn     =  DCTncn
057400020424      * - Peso Kg danneggiato/mancanti
057500020424     c                   eval      PD3pkd     =  DCTpkd
057600020424      * - Pezzi danneggiati/mancanti
057700020424     c                   eval      PD3npz     =  DCTnpz
057800020424      * - Note
057900020424     c                   movel     DCSnot        PD3dpz
058000020424      *
058100020424      * Campi del rec. DN64PD4:
058200020424      * - Anomalia
058300020424if  1c                   if        A§TADdest  <> *blanks
058400020424     c                   movel     A§TADdest     PD4tad
058500020424x   1c                   else
058600020424     c                   movel     A§TADdesc     PD4tad
058700020424e   1c                   endif
058800020424      * - Note aggiuntive (descrizione anomalia)
058900020424     c                   movel     $NoteA(1)     PD4dan
059000020424     c                   movel(p)  $NoteA(2)     PD4kos
059100020424     c                   move      $NoteA(3)     PD4kos
059200020424      *
059300020424      * Campi del rec. DN64PD5:
059400020620      * - Importo rimborsato
059500030409     c***                eval      PD5ipr = %trim(%editc(DCLipl+DCLipt:'K'))
059600030409if  1c                   if        DCLipl < DCLipt
059700030409     c                   eval      PD5ipr = 'VEDI C.A.'
059800030409x   1c                   else
059900030409     c                   eval      PD5ipr = %trim(%editc(DCLipl:'K'))
060000030409e   1c                   endif
060100020424      * - Causale di chiusura
060200020620if  1c                   if        DCTcch     <> *blanks
060300020620     c                   eval      *in04      =  *on
060400020620     c                   eval      PD5cch     =  DCTcch
060500020620     c                   eval      PD5ccd     =  §CCHdesc
060600020424x   1c                   else
060700020620     c                   clear                   PD5cch
060800020620     c                   clear                   PD5ccd
060900020424e   1c                   endif
061000020424      *
061100020424      * STAMPA
061200020620      *
061300020424      * - apertura iniziale del prtf
061400020424if  2c                   if        $Stampa    =  *off
061500020424     c                   open      FIDN64P
061600020424e   2c                   endif
061700020424      * - testata
061800050711     c                   if        *IN41
061900020424     c                   write     DN64PT
062000020424     c                   eval      *IN41      =  *off
062100020424     c                   endif
062200020424      * - separatore
062300050711     c                   if        $Stampa    =  *on
062400020424     c                   write     DN64P1
062500020424     c                   endif
062600020424      * - dettaglio
062700020424     c                   write     DN64PD1
062800020424     c                   write     DN64PD2
062900020424     c                   write     DN64PD3
063000020424     c                   write     DN64PD4
063100020424     c                   write     DN64PD5
063200020424     c                   eval      $Stampa    =  *on
063300020620      *
063400050711e   1***c                   endif
063500020424      *
063600020424      * AGGIORNAMENTO DEL RECORD IN ESAME DEL FILE FNDCT13L
063700050711      *   (se NON ristampa)
063800020424      *
063900020424     c                   if        I63ris     =  *blanks
064000020424     c                   move      WoggiJUL      DCTper
064100020424     c                   update    FNDCT000
064200020424     c                   endif
064300020424      *
064400020424     c                   ENDSR
064500020424
064600020424      ****************************************************************
064700020424      * ROUTINE FINALE
064800020424      ****************************************************************
064900020424     c     SR_ENDPGM     BEGSR
065000020424      *
065100020424      * Segnalo la mancanza di dati da stampare
065200120424if  1c                   if             $Stampa  = *off
065300120424     c                             and  I63ele  <> 'F'
065400020424     c                   open      FIDN64P
065500020424     c                   write     DN64PT
065600020424     c                   write     DN64PD0
065700020424     c                   eval      $Stampa  = *on
065800020424e   1c                   endif
065900020424      *
066000020424      * Stampo "Fine Stampa"
066100020424if  1c                   if        $Stampa  = *on
066200020424     c                   write     DN64PE
066300020424e   1c                   endif
066400020424      *
066500020424      * Chiudo TIBS02R
066600020424     c                   clear                   TIBS02DS
066700020424     c                   movel     'C'           T02tla
066800020424     c                   call      'TIBS02R'
066900020424     c                   parm                    KPJBA
067000020424     c                   parm                    TIBS02DS
067100150212      *
067200150212      * Chiudo TRULDEPR
067300150212     c                   clear                   TRULDEPds
067400150212     c                   eval      iDEPtla = 'C'
067500150212     c                   call      'TRULDEPR'
067600150212     c                   parm                    TRULDEPds
067700020424      *
067800020424      * Chiudo archivi aperti
067900020424     c                   close     FNDCT13L
068000120424     c                   if        %open(WFDPD00F)
068100120424     c                   close     WFDPD00F
068200120424     c                   endif
068300020424     c                   if        $Stampa  = *on
068400020424     c                   close     FIDN64P
068500020424     c                   eval      $Stampa  = *off
068600020424     c                   endif
068700020424      *
068800020424      * Termino !
068900020424     c                   return
069000020424      *
069100020424     c                   ENDSR
069200020424
069300020424      ****************************************************************
069400020424      * RICHIAMO PGM RICERCA/CONTROLLO TABELLE
069500020424      ****************************************************************
069600020424     c     CallTIBS02    BEGSR
069700020424      *
069800020424     c                   movel     'C'           T02mod
069900020424     c                   movel     KNSIF         T02sif
070000020424      *
070100020424     c                   call      'TIBS02R'
070200020424     c                   parm                    KPJBA
070300020424     c                   parm                    TIBS02DS
070400020424      *
070500020424     c                   ENDSR
070600020424
070700020424      ****************************************************************
070800020424      * CONVERSIONE DATA DAL FORMATO AAAA/MM/GG AL FORMATO AA/GGG
070900020424      ****************************************************************
071000020424     c     CNVDAT        BEGSR
071100020424      *
071200020424     c                   clear                   WLBdat
071300020424     c                   eval      G02inv = WdataYMD
071400020424     c                   eval      G02err = '3'
071500020424     c                   call      'XSRDA8'
071600020424     c                   parm                    WLBDAT
071700020424      *
071800020424     c     *eur          move      G02dat        WdataJUL
071900020424      *
072000020424     c                   ENDSR
072100020424
072200020424      ****************************************************************
072300020424      * DEFINIZIONE KLIST
072400020424      ****************************************************************
072500020424     c     DefKLIST      BEGSR
072600020424      *
072700020424      * file TITAS30C
072800020424     c     K04DCT02      klist
072900020424     c                   kfld                    DCTaas
073000020424     c                   kfld                    DCTlnp
073100020424     c                   kfld                    DCTnrs
073200020424     c                   kfld                    DCTnsp
073300020424      *
073400020424      * file TITA430C
073500020424     c     K05TA430      klist
073600020424     c                   kfld                    DCTaas
073700020424     c                   kfld                    DCTlnp
073800020424     c                   kfld                    DCTnrs
073900020424     c                   kfld                    DCTnsp
074000020424     c                   kfld                    TA4trc
074100020424      * file FNDET01L
074200020424     c     K02DET01      klist
074300020424     c                   kfld                    DCTaae
074400020424     c                   kfld                    DCTnev
074500020424      * file FNDCS01L
074600020424     c     K07DCS01      klist
074700020424     c                   kfld                    DCStpd
074800020424     c                   kfld                    DCSnkt
074900020424     c                   kfld                    DCSstd
075000020424     c                   kfld                    DCSdim
075100020424     c                   kfld                    DCShim
075200020424     c                   kfld                    DCSnks
075300020424     c                   kfld                    DCStrc
075400020424      * file FNDCL01L
075500020424     c     K03DCL01      klist
075600020424     c                   kfld                    DCTaac
075700020424     c                   kfld                    DCTfil
075800020424     c                   kfld                    DCTnca
075900020424      *
076000020424     c                   ENDSR
076100120424      /free
076200120424       //--------------------------------------------------------------
076300120424       //?Esecuzione del comando (già impostato).                      ?
076400120424       //--------------------------------------------------------------
076500120424       BEGSR  sr_ExecCmd;
076600120424
076700120424         clear Qcap0100;
076800120424         Qcabcsdh = *off;
076900120424         Qcapa    = *off;
077000120424         Qcacmdss = *off;
077100120424         Qcaerved = *allX'00';
077200120424
077300120424         clear Qusec;
077400120424         Qusbprv  = %size(Qusec);
077500120424
077600120424         ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
077700120424                           %size(Qcap0100) : 'CPOP0100' : *omit :
077800120424                           0 : 0 : Qusec);
077900120424
078000120424         //if  Qusei <> *blank;
078100120424         //  ...;
078200120424         //endif;
078300120424
078400120424       ENDSR;
078500120424
078600120424       //--------------------------------------------------------------
078700120424       //?Stampa segnalazione dell'errore rilevato.                    ?
078800120424       //--------------------------------------------------------------
078900120424       BEGSR  sr_PrintErr;
079000120424
079100120424         // -?Stampa del Dump?
079200120424         Dump(A);
079300120424
079400120424         // -?Stampa del Job-Log?
079500120424         Qcmd = 'DSPJOBLOG job(*) output(*print)';
079600120424         exsr  sr_ExecCmd;
079700120424
079800120424       ENDSR;
079900120424      /end-free
