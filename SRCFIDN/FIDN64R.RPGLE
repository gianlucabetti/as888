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
002000020424     FTITAS30C  IF   E           K DISK
002100171115     fFIPND01L  if   e           k disk
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
004700020424      * Flag operativi - campo DCTFLO del file FNDCT00F
004800020424     D DDCT01        e ds                  inz
004900171115      *
005000171115      * - Flag etichetta DPD
005100171115     d dPNDdp5       e ds                  qualified         inz
005200020424      *
005300020424      * Tabelle
005400020424     D TIBS02DS      e ds                  inz
005500020424     D DTADdct       e ds                  extname(DTAD)     inz
005600020424     D                                     prefix(A)
005700020424     D DTADdet       e ds                  extname(DTAD)     inz
005800020424     D                                     prefix(E)
005900020424     D DCCH          e ds                  inz
006000020424      *
006100020424      * Ridefinizione tracciati record
006200020424     D DS_TITAS      e ds                  extname(TITAS30C) inz
006300020424     D DS_TNTMD      e ds                  extname(TNTMD00F) inz
006400020424     D DS_FNDCS      e ds                  extname(FNDCS00F) inz
006500020424     D DS_FNDCL      e ds                  extname(FNDCL00F) inz
006900150212      *
007000150212      * -?Reperimento cod.cliente di un depot DPD?
007100150212     d TRULDEPds     e ds                  inz
007200171115     d TRULDEPds1    e ds                  qualified         inz
007300020424      *
007400020424      *------------- DS DI STATO
007500020424     D STATUS         sds
007600020424     D  VTCPGM           *proc
007700020424      *
007800020424      *------------- DS INTERNE
007900020424      * - Controllo/Inversione date
008000020424     D WLBDAT          ds                  inz
008100020424     D  G02DAT                 1      8  0
008200020424     D  G02INV                 9     16  0
008300020424     D  G02ERR                17     17
008400020424     D  G02TGI                18     22  0
008500020424      * - Chiave tipo descrizione per file FNDCS01L
008600020424     D DSnumCA         ds                  inz
008700020424     D  DCTaac
008800020424     D  DCTfil
008900020424     D  DCTnca
009000020424      *
009100020424      *------------- VARIABILI
009200020424      * - data in formato AAAAMMGG
009300020424     D WdataYMD        s              8  0 inz
009400020424      * - data in formato GIULIANO
009500020424     D WdataJUL        s               d   inz datfmt(*JUL)
009600020424      * - data odierna in formato GIULIANO
009700020424     D WoggiJUL        s               d   inz datfmt(*JUL)
009800020424      * - campo di comodo per memorizzazione DCTPER
009900020424     D WdctPER         s                   inz like(DCTper)
010000120424      *
010100020424      * - indice schiera
010200020424     D XX              s              3  0 inz
010300120424      *
010400020424      * - flag record da elaborare
010500020424     D $OKrec          s              1    inz(*off)
010600020424      * - flag stampato almeno un record
010700020424     D $Stampa         s              1    inz(*off)
010800020424      *
010900020424      *------------- COSTANTI
011000020619      * - causale di chiusura C.A.
011100020619     D Wchca           c                   const(900)
011200120424
012000120424       //--------------------------------------------------------------
012100120424       //?Definizione prototipi procedure.                             ?
012200120424       //--------------------------------------------------------------
012300120424
012400120424       // -?Parametri API QCAPCMD (Process Commands)?
012500120424     d Qcmd            s           2048    inz  varying
012600120424      /copy qSysInc/qRpgleSrc,QCAPCMD
012700120424       // -?API QCAPCMD (Process Commands)?
012800120424      /copy gaitrasrc/srcProtoPR,QCAPCMD
012900120424
013000120424       // -?Parametri gestione errori API.?
013100120424      /copy qsysinc/qrpglesrc,QUSEC
013200020424
013300020424      ****************************************************************
013400020424      *                   C   I   C   L   O                          *
013500020424      ****************************************************************
013600020424
013700020424     c     *ENTRY        PLIST
013800020424     c                   PARM                    KPJBA
013900020424      *
014000020424      * operazioni iniziali
014100020424     c                   exsr      SR_INZSR
014200020424      *
014300020424      * mi posiziono con la data sul file testate C.A.
014400020424     c                   if        I63ris   = 'S'
014500020424     c                   eval      *in01    = *on
014600020424     c                   move      WdataJUL      WdctPER
014700020424     c                   else
014800020424     c                   eval      *in01    = *off
014900020424     c                   eval      WdctPER  = *all'9'
015000020424     c                   endif
015100020424     c     WdctPER       setll     FNDCT000
015200020424      *
015300020424      * ciclo di lettura del file principale
015400020424      *
015500020424do  1c                   do        *hival
015600020424      *
015700020424     c     WdctPER       reade     FNDCT000
015800020424      *
015900020424      * e.o.f.
016000020424if  2c                   if        %eof(FNDCT13L)
016100020424     c                   leave
016200020424e   2c                   endif
016300020424      *
016400020424      * selezione record
016500020424     c                   exsr      SR_SELEZ
016600020424if  2c                   if        $OKrec = *off
016700020424     c                   iter
016800020424e   2c                   endif
016900020424      *
017000020424      * recupero dei dati aggiuntivi per la stampa
017100020424     c                   exsr      SR_DATI
017200120424      *
017300120424      * scrittura WrkF
017400120424     c                   if        I63ele <> 'S'
017500120424     c                   exsr      SR_WRITE
017600120424     c                   endif
017700020424      *
017800020424      * stampa
017900120424     c                   if        I63ele <> 'F'
018000020424     c                   exsr      SR_STAMPA
018100120424     c                   endif
018200020424      *
018300020424e   1c                   enddo
018400020424      *
018500020424      * Fine
018600020424     c                   exsr      SR_ENDPGM
018700020424
018800020424      ****************************************************************
018900020424      * ROUTINE INIZIALE
019000020424      ****************************************************************
019100020424     c     SR_INZSR      BEGSR
019200020424      *
019300120424     c                   eval      *INLR    = *on
019400020424     c                   movel     KPJBU         FIDN63DS
019500020424      *
019600050711      * Reperisco nome azienda per stampa PREPAGATI
019700050711     c     *dtaara       define    §azute        AZUTEds
019800050711     c     *dtaara       define    §datiute      dDATIUTE
019900050711     c                   in(E)     *dtaara
020000050711     c                   if        %ERROR or RSUT = *blanks
020100050711     c                   clear                   Tibs34Ds
020200050711     c                   call      'TIBS34R'
020300050711     c                   parm                    Tibs34Ds
020400050711     c                   in        *dtaara
020500050711     c                   endif
020600020424      *
020700020424     c                   move      *date         WoggiJUL
020800020424      *
020900050711     c                   eval      *in41    = *on
021000020424      *
021100020424if  1c                   if        I63ris   = 'S'
021200020424     c                   eval      WdataYMD = I63dat
021300020424     c                   exsr      CnvDat
021400020424     c                   z-add     G02dat        PTdata
021500020424     c***                move      WdataJUL      WdctPER
021600020424x   1c                   else
021700020424     c                   move      *date         PTdata
021800020424e   1c                   endif
021900120424      *
022000120424if  1c                   If        I63ele <> 'S'
022100120424      *
022200120424     c                   eval      Qcmd = 'CLRPFM file('
022300120424if  2c                   if        %subst( KNSIF : 7 : 1 ) = 'P'
022400120424     c                   eval      Qcmd += 'GAITRAAZP'
022500120424x   2c                   else
022600120424     c                   eval      Qcmd += 'GAITRAAZM'
022700120424e   2c                   endif
022800120424     c                   eval      Qcmd += '/WFDPD00F)'
022900120424      *
023000120424     c                   exsr      sr_ExecCmd
023100120424      *
023200120424if  2c                   if        Qusei <> *blank
023300120424     c                   exsr      sr_PrintErr
023400120424     c                   return
023500120424e   2c                   endif
023600120424      *
023700120424     c                   open      WFDPD00F
023800120424      *
023900120424e   1c                   EndIf
024000020424      *
024100020424     c                   ENDSR
024200020424
024300020424      ****************************************************************
024400020424      * SELEZIONE DEI RECORD DA ELABORARE
024500020424      ****************************************************************
024600020424     c     SR_SELEZ      BEGSR
024700020424      *
024800020424     c                   reset                   $OKrec
024900021120      *
025000021120      * Leggo una v.l. che prevede già:
025100021120      * - la OMIT dei record con il campo DCTper = *zeros
025200021120      * Tale campo è impostato dagli appositi programmi SOLO per:
025300021120      * - spedizioni Porto Franco, export DPD
025400021120      *              (§DCTport = 'F', §DCTtisp = 'E' e §DCTeuro = 'D')
025500021120      * - spedizioni export FEDEX    (§DCTtisp = 'E' e §DCTeuro = 'F')
025600021120      * - comunque con responsabilità PARTNER           (DCTres = 'P')
025700021120      * - e con tipo gestione "Pratica Assicurativa"    (DCTfpr = 'P')
025800020424      *
025900020424     c                   movel     DCTflo        dDCT01
026000020424      *
026100020424      * record annullati
026200020424     c     DCTatb        cabne     *blanks       EndSelez
026300020424      * Network estero non DPD
026400021120     c     §DCTeuro      cabne     'D'           EndSelez
026500020619      * Fase 900 (se NON RIstampa)
026600020619if  1c                   if            I63ris <> 'S'
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
028700171115      * Aggancio file estensione DPD
028800171116     c                   clear                   PNDaas
028801171116     c                   clear                   PNDlnp
028802171116     c                   clear                   PNDnrs
028803171116     c                   clear                   PNDnsp
028900171116     c                   eval      PNDaas = DCTaas
029000171116     c                   eval      PNDlnp = DCTlnp
029100171116     c                   eval      PNDnrs = DCTnrs
029200171116     c                   eval      PNDnsp = DCTnsp
029300171115     c     keyFIPND01    chain     FIPND000
029400171115if  1c                   if        %found( FIPND01L )
029500171115     c                   movel     PNDetiU       dPNDdp5
029600020424x   1c                   else
029700171115     c                   clear                   dPNDdp5
029800020424e   1c                   endif
029900020424      *
030000020424      * Aggancio file mittenti/destinatari
030100020424     c     K04DCT02      chain     TNTMD000
030200020424if  1c                   if        not %found(TNTMD01L)
030300020424     c                   clear                   DS_TNTMD
030400020424e   1c                   endif
030500150212      * -?Reperimento codice cliente DPD?
030600150212      *  ?Con il depot richiamo pgm per recuperare il "vero" cod.cliente?
030700150212     c                   clear                   TRULDEPds
030800171115     c                   clear                   TRULDEPds1
030900171115if  1c                   if        PNDida7  > *zeros
031000171115     c                   eval      TRULDEPds1.iDEPdep7 = PNDida7
031100171115x   1c                   else
031200171115     c                   eval      iDEPdpc  = dPNDdp5.§PNDidaSt
031300171115e   1c                   endif
031400171115     c*//*               eval      iDEPdrf = ( DCTaas * 10000 ) + TASmgs
031500150212     c                   call      'TRULDEPR'
031600150212     c                   parm                    TRULDEPds
031700171115     c                   parm                    TRULDEPds1
031800150212     c                   if        oDEPerr = *on
031900150212     c                   clear                   oDEPksc
032000150212     c                   endif
032100020424      *
032200020424      * Aggancio testata evento danni
032300020424if  1c                   if        DCTnev > *zeros
032400020424     c                   eval      *in03  = *on
032500020424     c     K02DET01      chain     FNDET000
032600020424if  2c                   if        not %found(FNDET01L)
032700020424     c                             or  DETatb <> *blanks
032800020424     c                   clear                   DETtad
032900020424e   2c                   endif
033000020424x   1c                   else
033100020424     c                   eval      *in03  = *off
033200020424     c                   clear                   DETtad
033300020424e   1c                   endif
033400020424      *
033500020424      * Aggancio file descrizioni per descrizione anomalia
033600020424     c                   clear                   XX
033700020424     c                   clear                   $NoteA
033800020424     c                   clear                   DS_FNDCS
033900020424     c                   movel     'C'           DCStpd
034000020424     c                   movel(p)  DSnumCA       DCSnkt
034100020424     c                   movel     'A'           DCSstd
034200020424     c                   clear                   DCSdim
034300020424     c                   clear                   DCShim
034400020424     c                   clear                   DCSnks
034500020424     c                   movel     'D'           DCStrc
034600020424     c     K07DCS01      chain     FNDCS000
034700020424if  1c                   if        %found(FNDCS01L)
034800020424do  2c                   dou           %eof(FNDCS01L)
034900020424     c                             or  XX >= 3
035000020424if  3c                   if        DCSatb =  *blanks
035100020424     c                   add       1             XX
035200020424     c                   movel     DCSnot        $NoteA(XX)
035300020424e   3c                   endif
035400020424     c     K07DCS01      reade     FNDCS000
035500020424e   2c                   enddo
035600020424e   1c                   endif
035700020424      *
035800020424      * Aggancio file descrizioni per descrizione pezzi mancanti
035900020424     c                   clear                   DS_FNDCS
036000020424if  1c                   if        DCTnpz > *zeros
036100020424     c                   movel     'C'           DCStpd
036200020424     c                   movel(p)  DSnumCA       DCSnkt
036300020424     c                   movel     'P'           DCSstd
036400020424     c                   clear                   DCSdim
036500020424     c                   clear                   DCShim
036600020424     c                   clear                   DCSnks
036700020424     c                   movel     'D'           DCStrc
036800020424     c     K07DCS01      chain     FNDCS000
036900020424do  1c                   if        not %found(FNDCS01L)
037000020424     c                             or  DCSatb <> *blanks
037100020424     c                   clear                   DCSnot
037200020424e   1c                   endif
037300020424e   1c                   endif
037400020424      *
037500020424      * Aggancio file liquidazione C.A.
037600020424     c     K03DCL01      chain     FNDCL000
037700020424if  1c                   if        not %found(FNDCL01L)
037800020424     c                             or  DCLatb <> *blanks
037900020424     c                   clear                   DS_FNDCL
038000020424e   1c                   endif
038100020424      *
038200020424      * Decodifico tipo anomalia
038300020424     c                   clear                   TIBS02DS
038400020424     c                   movel     'TAD'         T02cod
038500020424     c                   movel(p)  DCTtad        T02ke1
038600020424     c                   exsr      CallTIBS02
038700020424     c                   movel     T02uni        DTADdct
038800020424      *
038900020424      * Decodifico evento
039000020424     c                   clear                   DTADdet
039100020424if  1c                   if        *in03
039200020424     c                   clear                   TIBS02DS
039300020424     c                   movel     'TAD'         T02cod
039400020424     c                   movel     DETtad        T02ke1
039500020424     c                   exsr      CallTIBS02
039600020424     c                   movel     T02uni        DTADdet
039700020424e   1c                   endif
039800020424      *
039900020424      * Decodifico eventuale causale di chiusura
040000020424     c                   clear                   DCCH
040100020620     c                   eval      *in04      =  *off
040200020620if  1c                   if        DCTcch   <> *blanks
040300020620     c                   eval      *in04      =  *on
040400020424     c                   clear                   TIBS02DS
040500020424     c                   movel     'CCH'         T02cod
040600020424     c                   movel(p)  DCTcch        T02ke1
040700020424     c                   exsr      CallTIBS02
040800020424     c                   movel     T02uni        DCCH
040900020424e   1c                   endif
041000020424      *
041100020424     c                   ENDSR
041200120424      *
041300120424      ****************************************************************
041400120424      * SCRIVO IL RECORD DI LAVORO
041500120424      ****************************************************************
041600120424     c     SR_WRITE      BEGSR
041700120424      *
041800120424      * IMPOSTAZIONE CAMPI DEL FILE
041900120424     c                   clear                   WFDPD000
042000120424      *
042100120424      * - Estrazione
042200120424     c                   eval      DPDute     =  KNmUs
042300120424     c                   eval      DPDdtec    =  *date
042400120424      *
042500120424      * - Comunicazione Anomalia
042600120424     c                   eval      DPDaac     =  DCTaac
042700120424     c                   eval      DPDfil     =  DCTfil
042800120424     c                   eval      DPDnca     =  DCTnca
042900120424      * - Avviso Presunto Danno
043000120424     c                   eval      DPDdit     =  DCTdit
043100120424     c                   eval      DPDprn     =  DCTprn
043200120424     c                   eval      DPDpra     =  DCTpra
043300120424      * - Evento
043400120424if  1c                   if        *in03
043500120424     c                   eval      DPDaae = DCTpra
043600120424     c                   eval      DPDnev = DCTnca
043700120424if  2c                   if        E§TADdest  <> *blanks
043800120424     c                   movel     E§TADdest     DPDtae
043900120424x   2c                   else
044000120424     c                   movel     E§TADdesc     DPDtae
044100120424e   2c                   endif
044200120424e   1c                   endif
044300150212      * -?Codice cliente DPD?
044400150212     c                   eval      DPDksc     =  oDEPksc
044500120424      * - Codice DEPOT
044600171116     c                   if            PNDida7 = *blanks
044700171116     c                             or  PNDida7 = *zeros
044800171115     c                   eval      DPDida     =  dPNDdp5.§PNDidaSt
044900171116     c                   else
045000171116     c                   eval      DPDida7    =  PNDida7
045100171116     c                   endif
045200120424      * - Numero PARCEL
045300171115     c                   eval      DPDipn     =  PNDipn
045400120424      *   se lungo 11 aggiungo chk digits
045500171115     c                   if        %subst(DPDipn:12:1) = ' '
045600171115     c                   eval      %subst(DPDipn:12:1) = dPNDdp5.§PNDicd
045700120424     c                   endif
045800120424      * - Spedizione
045900120424     c                   eval      DPDlnp     =  DCTlnp
046000120424     c                   eval      DPDnrs     =  DCTnrs
046100120424     c                   eval      DPDnsp     =  DCTnsp
046200120424     c                   z-add     TASmgs        WdataYMD
046300120424     c                   movel     DCTaas        WdataYMD
046400120424     c                   exsr      CNVDAT
046500120424     c                   z-add     G02dat        DPDdsp
046600120424      * - Mittente
046700120424     c                   movel     TMDrsm        DPDrsm
046800120424     c                   movel     TMDinm        DPDinm
046900120424     c                   movel     TMDcam        DPDcam
047000120424     c                   movel     TMDlom        DPDlom
047100120424     c                   movel     TMDprm        DPDprm
047200120424     c                   movel     TMDnzm        DPDnam
047300120424      * - Destinatario
047400120424     c                   movel     TMDrsd        DPDrsd
047500120424     c                   movel     TMDind        DPDind
047600120424     c                   movel     TMDcad        DPDcad
047700120424     c                   movel     TMDlod        DPDlod
047800120424     c                   movel     TMDprd        DPDprd
047900120424     c                   movel     TMDnzd        DPDnad
048000120424      *
048100120424      * - Colli in bolla
048200120424     c                   eval      DPDncl     =  TASncl
048300120424      * - Peso Kg in bolla
048400120424     c                   eval      DPDpkf     =  TASpkf
048500120424      *
048600120424      * - Colli danneggiati/mancanti
048700120424     c                   eval      DPDncn     =  DCTncn
048800120424      * - Peso Kg danneggiato/mancanti
048900120424     c                   eval      DPDpkd     =  DCTpkd
049000120424      * - Pezzi danneggiati/mancanti
049100120424     c                   eval      DPDnpz     =  DCTnpz
049200120424      * - Note
049300120424     c                   movel     DCSnot        DPDdpz
049400120424      *
049500120424      * - Anomalia
049600120424if  1c                   if        A§TADdest  <> *blanks
049700120424     c                   movel     A§TADdest     DPDtad
049800120424x   1c                   else
049900120424     c                   movel     A§TADdesc     DPDtad
050000120424e   1c                   endif
050100120424      * - Note aggiuntive (descrizione anomalia)
050200120424     c                   movel     $NoteA(1)     DPDdan
050300120424     c                   movel(p)  $NoteA(2)     DPDkos
050400120424     c                   move      $NoteA(3)     DPDkos
050500120424      *
050600120424      * - Importo rimborsato
050700120424if  1c                   if        DCLipl < DCLipt
050800120424     c                   eval      DPDipr = 'VEDI C.A.'
050900120424x   1c                   else
051000120424     c                   eval      DPDipr = %trim(%editc(DCLipl:'K'))
051100120424e   1c                   endif
051200120424      * - Causale di chiusura
051300120424if  1c                   if        DCTcch     <> *blanks
051400120424     c                   eval      DPDcch     =  DCTcch
051500120424     c                   eval      DPDccd     =  §CCHdesc
051600120424x   1c                   else
051700120424     c                   clear                   DPDcch
051800120424     c                   clear                   DPDccd
051900120424e   1c                   endif
052000120424      *
052100120424      * SCRITTURA DLE RECORD
052200120424     C                   WRITE     WFDPD000
052300120424      *
052400120424     c                   ENDSR
052500020424
052600020424      ****************************************************************
052700020424      * STAMPA
052800020424      ****************************************************************
052900020424     c     SR_STAMPA     BEGSR
053000020424      *
053100020424      * IMPOSTAZIONE CAMPI IN STAMPA
053200020424      *
053300050711      * Campi del rec. DN64PD1:
053400020424      * - Avviso Presunto Danno
053500020424     c                   if           DCTdit  <> *blanks
053600020424     c                             or DCTprn  <> *zeros
053700020424     c                             or DCTpra  <> *zeros
053800020424     c                   eval      *in02      =  *on
053900020424     c                   eval      PD1dit     =  DCTdit
054000020424     c                   eval      PD1prn     =  DCTprn
054100020424     c                   eval      PD1pra     =  DCTpra
054200020424     c                   else
054300020424     c                   eval      *in02      =  *off
054400020424     c                   endif
054500020424      * - Comunicazione Anomalia
054600020424     c                   eval      PD1fil     =  DCTfil
054700020424     c                   eval      PD1nca     =  DCTnca
054800020424     c                   eval      PD1aac     =  DCTaac
054900150212      * -?Codice cliente DPD?
055000150212     c                   eval      PD1ksc     =  oDEPksc
055100020424      * - Codice DEPOT
055200171116     c                   if            PNDida7 = *blanks
055300171116     c                             or  PNDida7 = *zeros
055400171116     c                   eval      PD1depot   =  dPNDdp5.§PNDidaSt
055500171116     c                   else
055600171116     c                   eval      PD1depot   =  PNDida7
055700171116     c                   endif
055800020424      * - Numero PARCEL
055900171116     c                   eval      PD1parcel  =  PNDipn
056000060619     c* se lungo 11 aggiungo chk digits
056100171116     c                   if        %subst(PNDipn:12:1) = ' '
056200171116     c                   eval      %subst(PD1parcel:12:1) = dPNDdp5.§PNDicd
056300171116     c                   endif
056400020424      * - Spedizione
056500020424     c                   eval      PD1lnp     =  DCTlnp
056600020424     c                   eval      PD1nrs     =  DCTnrs
056700020424     c                   eval      PD1nsp     =  DCTnsp
056800020424     c                   z-add     TASmgs        WdataYMD
056900020424     c                   movel     DCTaas        WdataYMD
057000020424     c                   exsr      CNVDAT
057100020424     c                   z-add     G02dat        PD1dsp
057200020424      * - Mittente
057300020424     c                   movel     TMDrsm        PD1rsm
057400020424     c                   movel     TMDinm        PD1inm
057500020424     c                   movel     TMDcam        PD1cam
057600020424     c                   movel     TMDlom        PD1lom
057700020424     c                   movel     TMDprm        PD1prm
057800020424     c                   movel     TMDnzm        PD1nzm
057900020424      * - Destinatario
058000020424     c                   movel     TMDrsd        PD1rsd
058100020424     c                   movel     TMDind        PD1ind
058200020424     c                   movel     TMDcad        PD1cad
058300020424     c                   movel     TMDlod        PD1lod
058400020424     c                   movel     TMDprd        PD1prd
058500020424     c                   movel     TMDnzd        PD1nzd
058600020424      *
058700020424      * Campi del rec. DN64PD2:
058800020424      * - Colli in bolla
058900020424     c                   eval      PD2ncl     =  TASncl
059000020424      * - Peso Kg in bolla
059100020424     c                   eval      PD2pkf     =  TASpkf
059200020424      * - Evento
059300020424if  1c                   if        *in03
059400020424     c                   eval      PD2kev     =  %editc(DCTpra:'Z') + '/' +
059500020424     c                                        %trim(%editc(DCTnca:'Z'))
059600020424if  2c                   if        E§TADdest  <> *blanks
059700020424     c                   movel     E§TADdest     PD2tae
059800020424x   2c                   else
059900020424     c                   movel     E§TADdesc     PD2tae
060000020424e   2c                   endif
060100020424e   1c                   endif
060200020424      *
060300020424      * Campi del rec. DN64PD3:
060400020424      * - Colli danneggiati/mancanti
060500020424     c                   eval      PD3ncn     =  DCTncn
060600020424      * - Peso Kg danneggiato/mancanti
060700020424     c                   eval      PD3pkd     =  DCTpkd
060800020424      * - Pezzi danneggiati/mancanti
060900020424     c                   eval      PD3npz     =  DCTnpz
061000020424      * - Note
061100020424     c                   movel     DCSnot        PD3dpz
061200020424      *
061300020424      * Campi del rec. DN64PD4:
061400020424      * - Anomalia
061500020424if  1c                   if        A§TADdest  <> *blanks
061600020424     c                   movel     A§TADdest     PD4tad
061700020424x   1c                   else
061800020424     c                   movel     A§TADdesc     PD4tad
061900020424e   1c                   endif
062000020424      * - Note aggiuntive (descrizione anomalia)
062100020424     c                   movel     $NoteA(1)     PD4dan
062200020424     c                   movel(p)  $NoteA(2)     PD4kos
062300020424     c                   move      $NoteA(3)     PD4kos
062400020424      *
062500020424      * Campi del rec. DN64PD5:
062600020620      * - Importo rimborsato
062700030409     c***                eval      PD5ipr = %trim(%editc(DCLipl+DCLipt:'K'))
062800030409if  1c                   if        DCLipl < DCLipt
062900030409     c                   eval      PD5ipr = 'VEDI C.A.'
063000030409x   1c                   else
063100030409     c                   eval      PD5ipr = %trim(%editc(DCLipl:'K'))
063200030409e   1c                   endif
063300020424      * - Causale di chiusura
063400020620if  1c                   if        DCTcch     <> *blanks
063500020620     c                   eval      *in04      =  *on
063600020620     c                   eval      PD5cch     =  DCTcch
063700020620     c                   eval      PD5ccd     =  §CCHdesc
063800020424x   1c                   else
063900020620     c                   clear                   PD5cch
064000020620     c                   clear                   PD5ccd
064100020424e   1c                   endif
064200020424      *
064300020424      * STAMPA
064400020620      *
064500020424      * - apertura iniziale del prtf
064600020424if  2c                   if        $Stampa    =  *off
064700020424     c                   open      FIDN64P
064800020424e   2c                   endif
064900020424      * - testata
065000050711     c                   if        *IN41
065100020424     c                   write     DN64PT
065200020424     c                   eval      *IN41      =  *off
065300020424     c                   endif
065400020424      * - separatore
065500050711     c                   if        $Stampa    =  *on
065600020424     c                   write     DN64P1
065700020424     c                   endif
065800020424      * - dettaglio
065900020424     c                   write     DN64PD1
066000020424     c                   write     DN64PD2
066100020424     c                   write     DN64PD3
066200020424     c                   write     DN64PD4
066300020424     c                   write     DN64PD5
066400020424     c                   eval      $Stampa    =  *on
066500020620      *
066600050711e   1***c                   endif
066700020424      *
066800020424      * AGGIORNAMENTO DEL RECORD IN ESAME DEL FILE FNDCT13L
066900050711      *   (se NON ristampa)
067000020424      *
067100020424     c                   if        I63ris     =  *blanks
067200020424     c                   move      WoggiJUL      DCTper
067300020424     c                   update    FNDCT000
067400020424     c                   endif
067500020424      *
067600020424     c                   ENDSR
067700020424
067800020424      ****************************************************************
067900020424      * ROUTINE FINALE
068000020424      ****************************************************************
068100020424     c     SR_ENDPGM     BEGSR
068200020424      *
068300020424      * Segnalo la mancanza di dati da stampare
068400120424if  1c                   if             $Stampa  = *off
068500120424     c                             and  I63ele  <> 'F'
068600020424     c                   open      FIDN64P
068700020424     c                   write     DN64PT
068800020424     c                   write     DN64PD0
068900020424     c                   eval      $Stampa  = *on
069000020424e   1c                   endif
069100020424      *
069200020424      * Stampo "Fine Stampa"
069300020424if  1c                   if        $Stampa  = *on
069400020424     c                   write     DN64PE
069500020424e   1c                   endif
069600020424      *
069700020424      * Chiudo TIBS02R
069800020424     c                   clear                   TIBS02DS
069900020424     c                   movel     'C'           T02tla
070000020424     c                   call      'TIBS02R'
070100020424     c                   parm                    KPJBA
070200020424     c                   parm                    TIBS02DS
070300150212      *
070400150212      * Chiudo TRULDEPR
070500150212     c                   clear                   TRULDEPds
070600150212     c                   eval      iDEPtla = 'C'
070700150212     c                   call      'TRULDEPR'
070800150212     c                   parm                    TRULDEPds
070900020424      *
071000020424      * Chiudo archivi aperti
071100020424     c                   close     FNDCT13L
071200120424     c                   if        %open(WFDPD00F)
071300120424     c                   close     WFDPD00F
071400120424     c                   endif
071500020424     c                   if        $Stampa  = *on
071600020424     c                   close     FIDN64P
071700020424     c                   eval      $Stampa  = *off
071800020424     c                   endif
071900020424      *
072000020424      * Termino !
072100020424     c                   return
072200020424      *
072300020424     c                   ENDSR
072400020424
072500020424      ****************************************************************
072600020424      * RICHIAMO PGM RICERCA/CONTROLLO TABELLE
072700020424      ****************************************************************
072800020424     c     CallTIBS02    BEGSR
072900020424      *
073000020424     c                   movel     'C'           T02mod
073100020424     c                   movel     KNSIF         T02sif
073200020424      *
073300020424     c                   call      'TIBS02R'
073400020424     c                   parm                    KPJBA
073500020424     c                   parm                    TIBS02DS
073600020424      *
073700020424     c                   ENDSR
073800020424
073900020424      ****************************************************************
074000020424      * CONVERSIONE DATA DAL FORMATO AAAA/MM/GG AL FORMATO AA/GGG
074100020424      ****************************************************************
074200020424     c     CNVDAT        BEGSR
074300020424      *
074400020424     c                   clear                   WLBdat
074500020424     c                   eval      G02inv = WdataYMD
074600020424     c                   eval      G02err = '3'
074700020424     c                   call      'XSRDA8'
074800020424     c                   parm                    WLBDAT
074900020424      *
075000020424     c     *eur          move      G02dat        WdataJUL
075100020424      *
075200020424     c                   ENDSR
075300020424
075400020424      ****************************************************************
075500020424      * DEFINIZIONE KLIST
075600020424      ****************************************************************
075700020424     c     DefKLIST      BEGSR
075800020424      *
075900020424      * file TITAS30C
076000020424     c     K04DCT02      klist
076100020424     c                   kfld                    DCTaas
076200020424     c                   kfld                    DCTlnp
076300020424     c                   kfld                    DCTnrs
076400020424     c                   kfld                    DCTnsp
076500020424      *
077300020424      * file FNDET01L
077400020424     c     K02DET01      klist
077500020424     c                   kfld                    DCTaae
077600020424     c                   kfld                    DCTnev
077700020424      * file FNDCS01L
077800020424     c     K07DCS01      klist
077900020424     c                   kfld                    DCStpd
078000020424     c                   kfld                    DCSnkt
078100020424     c                   kfld                    DCSstd
078200020424     c                   kfld                    DCSdim
078300020424     c                   kfld                    DCShim
078400020424     c                   kfld                    DCSnks
078500020424     c                   kfld                    DCStrc
078600020424      * file FNDCL01L
078700020424     c     K03DCL01      klist
078800020424     c                   kfld                    DCTaac
078900020424     c                   kfld                    DCTfil
079000020424     c                   kfld                    DCTnca
079001171116      * - file FIPND01L
079006171116     c     keyFIPND01    klist
079007171116     c                   kfld                    PNDaas
079008171116     c                   kfld                    PNDlnp
079009171116     c                   kfld                    PNDnrs
079010171116     c                   kfld                    PNDnsp
079100020424      *
079200020424     c                   ENDSR
079300120424      /free
079400120424       //--------------------------------------------------------------
079500120424       //?Esecuzione del comando (già impostato).                      ?
079600120424       //--------------------------------------------------------------
079700120424       BEGSR  sr_ExecCmd;
079800120424
079900120424         clear Qcap0100;
080000120424         Qcabcsdh = *off;
080100120424         Qcapa    = *off;
080200120424         Qcacmdss = *off;
080300120424         Qcaerved = *allX'00';
080400120424
080500120424         clear Qusec;
080600120424         Qusbprv  = %size(Qusec);
080700120424
080800120424         ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
080900120424                           %size(Qcap0100) : 'CPOP0100' : *omit :
081000120424                           0 : 0 : Qusec);
081100120424
081200120424         //if  Qusei <> *blank;
081300120424         //  ...;
081400120424         //endif;
081500120424
081600120424       ENDSR;
081700120424
081800120424       //--------------------------------------------------------------
081900120424       //?Stampa segnalazione dell'errore rilevato.                    ?
082000120424       //--------------------------------------------------------------
082100120424       BEGSR  sr_PrintErr;
082200120424
082300120424         // -?Stampa del Dump?
082400120424         Dump(A);
082500120424
082600120424         // -?Stampa del Job-Log?
082700120424         Qcmd = 'DSPJOBLOG job(*) output(*print)';
082800120424         exsr  sr_ExecCmd;
082900120424
083000120424       ENDSR;
083100120424      /end-free
