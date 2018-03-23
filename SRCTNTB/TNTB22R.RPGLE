000100120207     /*PRM  dbgview(*source)
000200120207     /*END
000300120207     h debug decedit('0,') datedit(*dmy/) option(*nodebugio)
000400120207     h dftactgrp(*no)
000500120207     h bnddir('TRUL')
000600020207      *--------------------------------------------------------------------------------------------*
000700020207      *        GESTIONE TABELLA 3NN PER AUTORIZZAZIONI VCONFERMA BOLLE                             *
000800020207      *--------------------------------------------------------------------------------------------*
000900110805      *?  ATTENZIONE!!  ?
001000110805      *?    Questa tabella è utilizzata anche dal pgm TNTA61R  ?
001100110805      *?    'Interrogazione abilitazioni clienti'              ?
001200110805      *?    In caso di aggiunta/modifica campi alla tabella    ?
001300110805      *?    verificare se sono validi per la visualizzazione   ?
001400110805      *?    da TA61                                            ?
001500020207      *--------------------------------------------------------------------------------------------*
001600020207      * Data base                                                                                  *
001700020207      *--------------------------------------------------------------------------------------------*
001800020207     FTnTb22d   CF   E             WORKSTN
001900020207     F                                     SFILE(tb22s1:nrr)
002000020207     Ftntbe01l  IF   E           K DISK
002100020207     Ftntbe02l  IF   E           K DISK    rename(tntbe000:tntbe002)
002200020207     FAzOrg01l  IF   E           K DISK
002300120207
002400120207       //--------------------------------------------------------------
002500120207       //?Definizione costanti.                                        ?
002600120207       //--------------------------------------------------------------
002700120207
002800120207       // -?Supporto Cliente a Bartolini "EasySpedWEB"?
002900120207     d c_Supp_ESWEB    c                   const('ESWEB')
003000120207
003100020207      *--------------------------------------------------------------------------------------------*
003200020207      * Data structure                                                                             *
003300020207      *--------------------------------------------------------------------------------------------*
003400020207      *---
003500020207      * Schiere a tempo di compilazione
003600020207      *---
003700020208     D msg             S             78    DIM(3) CTDATA PERRCD(1)              *Messaggi video
003800020207      *---
003900020207      * Schiere memorizzazione dati correnti
004000020207      *---
004100020207      * Variabili
004200020207      *---
004300020207     D Nrr             S              4  0                                      *n° rel sf
004400120207     D $Fine           S               n   INZ                                  *fine programma
004500020207     D TipVid          S              1    INZ('1')                             *video in gestione
004600020207     D $CarSfl         S              1    INZ('0')                             *video in gestione
004700020207     D $CarVd1         S              1    INZ('1')                             *video in gestione
004800020207      *---
004900020207      * Indici
005000020207      *---
005100020207     D i               S              5  0 INZ                                  *indice
005200020207     D j               S              5  0 INZ                                  *indice
005300020207     D k               S              5  0 INZ                                  *indice
005400020207     D g               S              5  0 INZ                                  *indice
005500020207     D v               S              5  0 INZ                                  *indice
005600020207     D m               S              5  0 INZ                                  *indice
005700020207     D W1Cod           S                   like(C1COld)
005800020207     D WMax            S                   like($nrr)
005900011010     D Win             S             99A
006000020207      *---
006100020207      * Ds
006200020207      *---
006300020207     D Tntb22Ds      E DS                                                       *ds di procedura
006400020207     D                                     INZ                                  *ds di procedura
006500020207     D Tibs02Ds      E DS                                                       *ds di procedura
006600020207     D Tibs69Ds      E DS                                                       *ds di procedura
006700020207     D Og143         E DS                                                       *ds di procedura
006800020207     D DsAco         E DS                  ExtName(CnAco00f)                    *ds di procedura
006900020207     D DsInd         E DS                  ExtName(CnInd00f)                    *ds di procedura
007000020207     D DsClp         E DS                  ExtName(CnClp00f)                    *ds di procedura
007100020207     D DsCls         E DS                  ExtName(FnCls00f)                    *ds di procedura
007200020207      *
007300020207      *---
007400020207      * architettura
007500020207      *---
007600020207     D kpjba         E DS                                                       *architettura
007700110805     d Tnta61ds      e ds
007800120207
007900120207       // -?Tabelle usate:?
008000120207       // ·?"3C" = Clienti che inviano bolle partenza?
008100120207     d ds3C          e ds                  inz  qualified
008200120207       // ·?"3EW" = Dati assegnati alla postazione EasyWeb?
008300120207     d d3EW          e ds                  inz  qualified
008400120207
008500120207       // -?Status ds?
008600120207     d Status         sds
008700120207     d  SDSpgm           *proc
008800120207
008900120207       //--------------------------------------------------------------
009000120207       //?Definizione aree dati.                                       ?
009100120207       //--------------------------------------------------------------
009200120207
009300120207       // -?Dati utente?
009400120207     d §AzUte        e ds                  extname(AZUTE00F)
009500120207     d                                     dtaara
009600120207     d §DatiUte      e ds                  extname(dDatiUte)
009700120207     d                                     dtaara
009800120207
009900120207       //--------------------------------------------------------------
010000120207       //?Definizione prototipi procedure.                             ?
010100120207       //--------------------------------------------------------------
010200120207
010300120207       // -?Reperimento dati utente?
010400120207     d TIBS34ds      e ds                  inz
010500120207      /copy gaitrasrc/srcProtoPR,TIBS34R
010600120207
010700120207       // -?Costante identificativa del file TABEL per *srvpgm TRULTAB?
010800120207     d c_TABEL         c                   const('1')
010900120207       // -?Costante identificativa del file TNTBE per *srvpgm TRULTAB?
011000120207     d c_TNTBE         c                   const('2')
011100120207       // -?Reperimento dati tabelle?
011200120207      /copy gaitrasrc/srcProtoPR,TRULTAB
011300120207
011400120207       // -?Verifica abilitazione STRATEGI?
011500120207      /copy gaitrasrc/srcProtoPI,TIS7701R
011600120207     d TIVSSds       e ds                  extname(TIVSS00F) qualified
011700120207      /copy gaitrasrc/srcProtoPR,TIS7701R
011800120207
011900020207      *--------------------------------------------------------------------------------------------*
012000020207      * Main lines
012100020207      *--------------------------------------------------------------------------------------------*
012200020207      *
012300020207      * Operazioni iniziali
012400020207      *
012500020207     C                   EXSR      RutInz
012600110805
012700110805     c                   eval      *in10 = *on
012800110805     c                   Select
012900110805     c                   When      %parms() > 1  and  TA61ksc <= *zero
013000110805     c                   eval      TA61err = *on
013100110805     c                   eval      TA61msg = 'Richiesto il codice cliente'
013200120207     c                   eval      $Fine = *on
013300110805     c                   When      %parms() > 1
013400110805     c                   eval      V1Ccod  = %editc(TA61ksc:'X')
013500110805     c                   eval      *in10 = *off
013600110805     c                   eval      TipVid = '2'
013700110805     c                   eval      $CarSfl = *on
013800110805     c                   eval      $CarVd1 = *off
013900110805     C                   move      v1ccod        $Cod
014000110805     C                   EXSR      DeCod
014100110805     c                   EndSl
014200020207      *
014300020207      * Gestione video
014400020207      *
014500120207     C     $Fine         DOUEQ     *on
014600020207     C     TipVid        CASEQ     '1'           GesVd1                          *Sfile
014700020207     C     TipVid        CASEQ     '2'           GesSfl                          *Sfile
014800010921     C                   ENDCS
014900010921     C                   ENDDO
015000020207      *
015100010921     C                   SETON                                        LR
015200020207      *--------------------------------------------------------------------------------------------*
015300020207      * GesVd1 - Gestione videata selezione codice tabella -
015400020207      *--------------------------------------------------------------------------------------------*
015500020207     C     GesVd1        BEGSR
015600020207      *
015700020207      * Inizializzazione videata
015800020207     C     $CarVd1       ifeq      *on
015900020207     C                   EXSR      CarVd1
016000020207     C                   move      *off          $CarVd1
016100020207     C                   endif
016200020207      *
016300120208     c                   if        Not *in28
016400120207     C                   write     TB22T1
016500120208     c                   endif
016600020208     C                   EXFMT     Tb22v1
016700020208      *
016800020208     C                   EXSR      CtrSearch
016900020207      *
017000020207     C                   select
017100020207      * F3=Fine
017200020207     C     *inKc         wheneq    *on
017300020207     C                   EXSR      F03Ges
017400020207      *
017500020207     C     *in55         wheneq    *on
017600020207     C                   EXSR      Search
017700020207     C                   other
017800020207      *
017900020207      * Controlli e decodifiche
018000020208     C                   EXSR      CtrV1
018100020207      *
018200020207     C     *IN28         ifeq      *off
018300020211     C     Ric_Est       andeq     *off
018400020207      *
018500020207      * Gestisco il video di dettaglio
018600020207     C                   move      *on           $CarSfl
018700020207     C                   move      '2'           TipVid
018800020207     C                   endif
018900020207      *
019000020207     C                   endsl
019100020207      *
019200020207      *
019300020207     C                   endsr
019400020207      *--------------------------------------------------------------------------------------------*
019500020207      * CarVd1 - Carica primo video
019600020207      *--------------------------------------------------------------------------------------------*
019700020207     C     CarVd1        BEGSR
019800020207      *
019900020207     C                   setoff                                       284041
020000020211      *
020100020207     C                   clear                   Tb22v1
020200020207     C                   movel     '?      '     V1CCod
020300020207      *
020400020207     C                   ENDSR
020500020207      *--------------------------------------------------------------------------------------------*
020600020207      * Search - Ricerca su campi video
020700020207      *--------------------------------------------------------------------------------------------*
020800020207     C     Search        BEGSR
020900020208     C                   setoff                                       284041
021000020207      *
021100020208      * Il P.O deve essere controllato
021200020208     C     v1cpou        chain     Azorg01l                           21
021300020208     C     v1cpou        ifeq      *zeros
021400020208     C     *in21         oreq      *off
021500020207      *
021600020207     C                   move      *blanks       V1DCod
021700020207     C                   move      *blanks       Tibs02Ds
021800020207     C                   move      '3NN'         T02Cod
021900020211      *
022000020211      * Gli come chiave parziale per le ricerche il P.O.
022100020211     C                   move      *blanks       t02ke1
022200020211     C                   movel     v1cpou        t02ke1
022300020207     C                   move      'R'           T02Mod
022400020207     C                   movel     Knsif         T02Sif
022500020207     C                   CALL      'TIBS02R'
022600020207     C                   PARM                    kpjba
022700020207     C                   PARM                    Tibs02Ds
022800020207      *
022900020207      * per valorizzare a video devo aver selezionato un codice
023000020207     C                   if        T02Err = *blanks and T02ke1<>*zeros
023100020208     C                   movel     T02ke1        v1ccod
023200020208      *
023300020208      * decodifico
023400020208     C                   move      v1ccod        $Cod              7 0
023500020208     C                   EXSR      DeCod
023600020208      *
023700020208     C                   if        o69err = *blanks and acoflg =' '
023800020208     C                   movel     acorag        v1dcod
023900020208     C                   endif
024000020207     C                   endif
024100020208     C                   endif
024200020207      *
024300020207     C                   ENDSR
024400020207      *--------------------------------------------------------------------------------------------*
024500020207      * Controllo e decodifica prima videata
024600020207      *--------------------------------------------------------------------------------------------*
024700020208     C     CtrV1         BEGSR
024800020207      *
024900020208     C                   setoff                                       284041
025000020211     C                   move      *off          Ric_Est           1
025100020207     C                   clear                   V1CMsg
025200020207      *
025300020208      * se avevo richiesto la ricerca effettuo subito i controlli su P.O.
025400020208 1a  C     *in56         ifeq      *on
025500020208 2a  C     v1cpou        ifne      *zeros
025600020208     C     v1cpou        chain     Azorg01l                           21
025700020208     C   21              seton                                        2841
025800020208     C   21              goto      FineSr
025900020208 2-  C                   endif
026000020208 1-  C                   endif
026100020208      *
026200020208 1a  C     v1ccod        ifeq      *blanks
026300020207      *
026400020207      * Ricerca estesa
026500020207     C
026600120207     C                   movel     rsut          paxdut           30
026700020207     C                   movel(P)  v1dcod        paxdmt           48
026800020207      *
026900020207     C                   z-add     0             paxsta
027000120207     C                   z-add     DUTkci        paxccc            4 0
027100020207     C                   clear                   paxdit
027200020207     C                   z-add     1             paxnum
027300020207      *
027400020207     C                   CALL      'XALFA3BR'
027500020207     C                   PARM                    PAXDUT
027600020207     C                   PARM                    CODUT
027700020207     C                   PARM                    PAXDMT
027800020207     C                   PARM                    PAXCCC
027900020207     C                   PARM                    PAXSTA            1 0
028000020207     C                   PARM                    PAXFLR           90
028100020207     C                   PARM                    PAXDIT            3
028200020207     C                   PARM                    PAXNUM            2 0
028300020207     C                   PARM                    PAXKCM           80
028400020207     C                   PARM                    PAXKSM          140
028500020207     C                   PARM                    PAXKDM           60
028600020207      *
0287000202082a   C     PaxSta        ifgt      -1
028800020207     C                   movel     paxksm        v1ccod
028900020207     C                   movel     paxdmt        v1dcod
0290000202082-   C                   endif
029100020211      * sehgnalo che provengo da ricerca estesa per
029200020211      * riemettere video.
029300020211     C                   move      *on           Ric_Est           1
029400020208     C                   goto      Finesr
0295000202081-   C                   endif
029600020207      *
029700020207      * Codice cliente obbligatorio
029800020208     C                   movel     *blanks       v1dcod
0299000202081a   C     v1ccod        ifeq      *blanks
030000020208     C                   seton                                        2840
030100020207      *
030200020207      * Se esiste controllo e decodifico
0303000202081e   C                   else
030400020208      * Controllo se il codice è numerico
030500020208     C                   testn                   v1ccod               31
030600020208     C                   move      v1ccod        w001a             1
0307000202082a   C     *in31         ifeq      *on
030800020208     C     w001a         andge     '0'
030900020208     C                   move      v1ccod        $Cod              7 0
031000020208     C                   EXSR      DeCod
031100020208      *
0312000202083a   C                   if        o69err = *blanks and acoflg =' '
031300020207     C                   movel     acorag        v1dcod
0314000202083e   C                   else
031500020208     C                   seton                                        2840
0316000202083-   C                   endif
031700020208      *
0318000202082e   C                   else
031900020208     C                   seton                                        2840
0320000202082-   C                   endif
0321000202081-   C                   endif
032200020207      *
032300020208     C     FineSr        ENDSR
032400020207      *--------------------------------------------------------------------------------------------*
032500020207      * Gessfl - gestione video subfile
032600020207      *--------------------------------------------------------------------------------------------*
032700020207     C     GesSfl        BEGSR
032800010924      *
032900010924      * azzero indicatori
033000020207     C     $CarSfl       ifeq      *on
033100020207     C                   exsr      CarSfl
033200010921     C                   endif
033300010927      *
033400120207      * scrivo la testata
033500120208     c                   if        Not *in28
033600120207     c                   write     TB22T1
033700120208     c                   endif
033800010927      * scrivo il piede
033900020207     C                   write     tb22z1
034000011017      * se non ci sono dati da caricare e non ho richesto il posizionamento
034100010927     C     wmax          ifeq      0
034200020207     C                   seton                                        71
034300020207     C                   setoff                                       70
034400020207     C                   write     tb22v2                                       NO RCD
034500011017     C                   endif
034600010927      * emetto il control
034700020207     C                   exfmt     tb22c1
034800020207      *
034900020207     C                   select
035000020207      *
035100020207      * F03=Fine
035200020207     C     *inkc         wheneq    *on
035300020207     C                   EXSR      F03Ges
035400020207      *
035500020207      * F10=Immissione
035600020207     C     *inkj         wheneq    *on
035700020211     C                   EXSR      F10Ges
035800020207      *
035900020207      * F12=Ritorno
036000020207     C     *inkl         wheneq    *on
036100020207     C                   EXSR      F12Ges
036200020207     C                   EndSl
036300010928      *
036400020207      * Ho qualcosa di caricato da controllare
036500020207     C     wmax          ifgt      *zeros
036600020207     C                   EXSR      CtrS1
036700010928     C                   endif
036800020207      *
036900010921     C                   ENDSR
037000020207      *--------------------------------------------------------------------------------------------*
037100020207      * CarSfl - caricamento sfile
037200020207      *--------------------------------------------------------------------------------------------*
037300020207     C     CarSfl        BEGSR
037400020207      *
037500010928     C                   setoff                                           90
037600020207     C                   z-add     *zeros        Nrr                            *n° record sfile
037700020207     C                   z-add     *zeros        c1cold                         *n° record sfile
037800020208     C                   move      *blanks       c1dold                         *n° record sfile
037900020207      * pulizia sfile
038000020207     C                   seton                                        7071      *non visualizza sfl
038100020207     C                   write     tb22c1
038200020207     C                   setoff                                       7071
038300020207      *
038400020207      * caricamento sfile -tutto-
038500020207     C                   EXSR      ScrSfl                                       *scrive sfile
038600020207      *
038700010921     C                   ENDSR
038800020207      *--------------------------------------------------------------------------------------------*
038900020207      * ScrSfl - scrive il sfile -tutto-
039000020207      *--------------------------------------------------------------------------------------------*
039100020207     C     ScrSfl        BEGSR
039200120207      *
039300120207      /free
039400120207
039500120207         // -?Reperimento tab. "3C"?
039600120207         clear  ds3C;
039700120207         clear  d3EW;
039800120207         ds_TNTBE.TBEke1 = V1Ccod;
039900120207         IF  getTabella ( c_Tabel : '3C'  : ds_TNTBE.TBEke1 :
040000120207                          *omit : *omit : *omit :
040100120207                          *omit : *omit :
040200120207                          *omit : *omit : *omit : *omit :
040300120207                          *omit : *omit : *omit : *omit :
040400120207                          ds_TNTBE.TBEatb : ds_TNTBE.TBEuni )
040500120207                        = *zero      AND
040600120207             ds_TNTBE.TBEatb = *blank;
040700120207           ds3C   = ds_TNTBE.TBEuni;
040800120207           If  ds3C.§3Ccba = c_Supp_ESWEB;
040900120207             // -?Reperimento abilitazione a Strategi (SUN)?
041000120418             //rqsKSU = '0' + V1Ccod;
041100120418             rqsKSU = '0' + %editc( ds3C.§3Ccks : 'X' );
041200120207             clear  rqsSUN;
041300120207             clear  rpyEsito;
041400120207             Selettore_Record_TIVSS ( 'GETRCDVSS' : '0' : rqsKSU : 'EW' :
041500120207                                      rqsSUN : *zero : TIVSSds :
041600120207                                      rpyEsito );
041700120207             // -?Reperimento tab. "3EW"?
041800120418             //ds_TNTBE.TBEke1 = '0' + V1Ccod;
041900120418             ds_TNTBE.TBEke1 = '0' + %editc( ds3C.§3Ccks : 'X' );
042000120207             clear  ds_TNTBE.TBEke2;
042100120207             if  rpyEsito = *zero  and  TIVSSds.VSSksu > *zero;
042200120207               ds_TNTBE.TBEke2 = TIVSSds.VSSsun;
042300120207             endif;
042400120207             if  getTabella ( c_Tntbe : '3EW' : ds_TNTBE.TBEke1 :
042500120207                              ds_TNTBE.TBEke2 : *blank : *blank :
042600120207                              *omit : *omit :
042700120207                              *omit : *omit : *omit : *omit :
042800120207                              *omit : *omit : *omit : *omit :
042900120207                              ds_TNTBE.TBEatb : ds_TNTBE.TBEuni )
043000120207                            = *zero      AND
043100120207                 ds_TNTBE.TBEatb = *blank;
043200120207               d3EW = ds_TNTBE.TBEuni;
043300120207             endif;
043400120207           EndIf;
043500120207         ENDIF;
043600120207
043700120207      /end-free
043800020207      *
043900020207      * imposta dati
044000020207     C                   move      v1ccod        c1cold
044100020208     C                   movel     v1dcod        c1dold
044200120816     c                   if        ds3C.§3Ccba  = c_Supp_ESWEB
044300120816     c                   if        d3EW.§3EWfaa = *blank
044400120207     c                   eval      C1Cfls = d3EW.§3EWfls
044500120816     c*//                eval      C1Cnrs = d3EW.§3EWnrs
044600120816     c                   endif
044700120207     c                   else
044800120207     c                   eval      C1Cfls = ds3C.§3Cfls
044900120816     c*//                eval      C1Cnrs = ds3C.§3Cnrs
045000120816     c                   endif
045100120816     c                   eval      C1Cnrs = ds3C.§3Cnrs
045200010927     C                   z-add     0             wmax
045300020207      *
045400020207     C                   move      '3NN'         tbecod
045500020208     C                   move      *blanks       tbelin
045600020207     C                   movel     $sif          tbesif
045700020207     C                   move      *blanks       tbeke1
045800020207     C                   movel     v1ccod        tbeke1
045900010927      *
046000010927      * mi posiziono anche per codice
046100020207     C     k02tbe4       setll     tntbe02l
046200020207     C     k02tbe4       reade     tntbe02l                               97
046300010924      *
046400010924      * Ciclo di lettura sulla tabella da caricare
046500020207     C                   dow       not *in97
046600010924      *
046700010924      * riempimento campi subfile
046800020207     C                   exsr      RieS1
046900011017      * imposto attributi su sfl
047000011017     C                   Exsr      AtrS1
047100010924      * scrive sfile
047200010924     C                   add       1             nrr                            *incrementa n°rec
047300010927      *
047400010927      * mi posiziono sul primo record caricato
047500010927     C                   z-add     1             $nrr                           *posiziona in fondo
047600020207     C                   write     tb22s1                                       *scrive sfile
047700020207     C                   z-add     nrr           wmax
047800010921      *
047900020207     C     k02tbe4       reade     tntbe02l                               97
048000020207     C                   ENDDO
048100020207     C                   move      '0'           $CarSfl
048200020207      *
048300010921     C                   ENDSR
048400020207      *--------------------------------------------------------------------------------------------*
048500020207      * F03ges - Tasto funzionale f03 -> Fine programma
048600020207      *--------------------------------------------------------------------------------------------*
048700020207     C     F03ges        BEGSR
048800020207      *
048900120207     C                   movel     *on           $Fine                          *fine pgm
049000020207      *
049100010921     C                   ENDSR
049200020211      *--------------------------------------------------------------------------------------------*
049300020211      * F10ges - Tasto funzionale f10 -> Immissione
049400020211      *--------------------------------------------------------------------------------------------*
049500020211     C     F10ges        BEGSR
049600020211      *
049700020211     C                   reset                   Tntb22ds
049800020214     C                   move      *blanks       b22ke1
049900020214     C                   movel     c1cold        b22ke1
050000020211     C                   eval      B22Fun='F10'
050100020211     C                   EXSR      callb23r
050200020211      *
050300020211     C                   move      *on           $CarSfl
050400020211     C                   z-add     1             $nrr                           *posiziona in fondo
050500020211      *
050600020211     C                   ENDSR
050700020207      *--------------------------------------------------------------------------------------------*
050800020207      * F12ges - Tasto funzionale f12 -> Ritorno
050900020207      *--------------------------------------------------------------------------------------------*
051000020207     C     F12ges        BEGSR
051100020207      *
051200110805     c                   IF        *in10
051300020207     C                   move      '1'           TipVid
051400020212     C                   move      *on           $CarVd1
051500110805     c                   else
051600120207     C                   movel     *on           $Fine                          *fine pgm
051700110805     c                   endif
051800020207      *
051900020207     C                   ENDSR
052000020207      *--------------------------------------------------------------------------------------------*
052100020207      * callb23r Richiamo tntb22r nelle diverse modalità
052200020207      *--------------------------------------------------------------------------------------------*
052300020207     C     callb23r      BEGSR
052400010924      *
052500020207      * valorizzo Ds di richiamo
052600010924     C
052700020207     C                   eval      b22lin=' '
052800020207     C                   eval      b22sif=tbesif
052900010924      *
053000010924      * Richiamo pgm di gestione
053100020207     C                   call      'TNTB23R'
053200010925     C                   PARM                    kpjba
053300020207     C                   PARM                    tntb22ds
053400010924      *
053500120207     C     b22fun        ifeq      'F03'
053600020207     C     b22err        oreq      *on
053700120207     C                   eval      $Fine = *on
053800120207     c                   else
053900120207     c                   if        C1Cnrs > *zero
054000120207     c                   clear                   kpjbu
054100120207     c                   call      'TRTB35R'
054200120207     c                   parm                    kpjba
054300120207     c                   endif
054400120207     C                   endif
054500020207      *
054600010924     C                   ENDSR
054700020207      *--------------------------------------------------------------------------------------------*
054800020207      * CtrS1  - Controlla campi sfile
054900020207      *--------------------------------------------------------------------------------------------*
055000020207     C     CtrS1         BEGSR
055100020207      *
055200011008     C                   setoff                                         9082
055300020208     C                   setoff                                         2842
055400010924      *
055500010924      * lettura dei record "toccati" del sfile
055600010924     C                   z-add     1             nrr
055700020207     C                   readc     tb22s1                                 82
055800010927     C                   dow       not *in82
055900010924      *
056000010924      * gestione opzioni
056100010924      *
056200010924      * Opzioni
056300020207     C                   if        s1copz <> ' '
056400010924      *
056500020207     C                   if        s1copz = '2'  and s1cAnn <>'A'or
056600020207     C                             s1copz = '4'  and s1cAnn <>'A'or
056700020207     C                             s1copz = '5'  or
056800020207     C                             s1copz = '7'  and s1cAnn ='A'
056900010926      *
057000020207     C                   reset                   tntb22ds
057100020207     C                   eval      b22opz=s1copz
057200020207      * campi chiave
057300020207     C                   move      *blanks       b22ke1
057400020207     C                   movel     c1cold        b22ke1
057500020207     C                   move      *blanks       b22ke2
057600020207     C                   movel     s1cpou        b22ke2
057700010924      *
057800020211      * Gestione richiamo tntb23r
057900020207     C                   EXSR      callb23r
058000020211      * ricarico il subfile
058100020211     C                   eval      $carsfl='1'
058200020211      * mi posiziono sul record da cui ho selezionato l'opzione
058300020211     C                   z-add     nrr           $nrr                           *posiziona in fondo
058400010924      *
058500020207     C                   else
058600010927      * c'e un errore sulle opzioni
058700020207     C                   seton                                        9082
058800020208     C                   seton                                        2842
058900020207     C                   exsr      ErrOpz
059000020207     C                   endif
059100020208     C                   endif
059200020207      *
059300020207      * rilegge sfile per sposizionamento
059400020207     C     nrr           chain     tb22s1                             97        *per sposizionamento
059500020207      *
059600020207      * aggiorna sfile
059700020207     C                   EXSR      aggsfl                                       *aggiorna sfile
059800020207      *
059900020207      * lettura successiva record
060000020207     C                   readc     tb22s1                                 82
060100010927     C                   enddo                                                  *fine sfile
060200020207      *
060300000000     C                   ENDSR
060400020207      *--------------------------------------------------------------------------------------------*
060500010927      * Errori su scelta opzione
060600020207      *--------------------------------------------------------------------------------------------*
060700010927     C     Erropz        BEGSR
060800020207      *
060900020207     C                   clear                   c1cmsg
061000020208      *
061100020207     C                   if        s1copz <>'2'  and s1copz <>'4'and
061200020207     C                             s1copz <>'5'  and s1copz <>'7'
061300020208     C                   eval      c1cmsg  = msg(1)
061400020207     C                   endif
061500010928      *
061600020207     C                   if        s1copz = '7'
061700020208     C                   eval      c1cmsg  = msg(2)
061800020207     C                   endif
061900010928      *
062000020207     C                   if        s1copz = '2'  or  s1copz = '4'
062100020208     C                   eval      c1cmsg  = msg(3)
062200020207     C                   endif
062300020207      *
062400020207     C                   ENDSR
062500020207      *--------------------------------------------------------------------------------------------*
062600020207      * aggsfl - aggiorna riga di sfile
062700020207      *--------------------------------------------------------------------------------------------*
062800000000     C     aggsfl        BEGSR
062900010924      * aggiorna sfile
063000020207     C                   Z-ADD     nrr           $nrr                           *posizionamento
063100020208     C                   seton                                        70
063200020208     C  n28              clear                   s1copz
063300011017      *
063400020215     C                   move      s1hflg        *in43
063500020207     C                   update    tb22s1
063600020207     C                   setoff                                       70
063700020207      *
063800000000     C                   ENDSR
063900020207      *--------------------------------------------------------------------------------------------*
064000020207      * RieS1  - vlorizzo i campi del subfile
064100020207      *--------------------------------------------------------------------------------------------*
064200020207     C     Ries1         BEGSR
064300020207      *
064400020208      * Nuovo codice cliente e decodifica
064500020207     C                   setoff                                       4041
064600020211     C                   move      ' '           s1copz
064700030704     C*    TbeUni        ifne      *blanks
064800030704     c                   IF        %SUBST(tbeuni:1:7) <> *blanks  and
064900030704     c                             %SUBST(tbeuni:1:7) <> *zeros
065000020208     C                   movel     tbeuni        s1cnew
065100020214     C                   else
065200020214     C                   move      *blanks       s1cnew
065300020214     C                   endif
065400020208      *
065500020208      * Se esiste controllo e decodifico
065600020214     C                   move      *blanks       s1dnew
065700020215     C     s1cnew        ifne      *blanks
065800020208     C                   move      s1cnew        $Cod              7 0
065900020208     C                   EXSR      DeCod
066000020208      *
066100020208     C                   if        o69err = *blanks and acoflg =' '
066200020208     C                   movel     acorag        s1dnew
066300020208     C                   endif
066400020215     C                   endif
066500020208      *
066600020208      * Punto operativo e decodifica
066700020207     C                   movel     tbeke2        s1cpou
066800020214     C                   movel     *blanks       s1dpou
066900020214     C     s1cpou        ifne      '999'
067000020207     C                   move      s1cpou        $Fil              3 0
067100020207     C     $Fil          chain     Azorg01l                           21
067200020207     C                   movel     orgdes        s1dpou
067300020214     C                   else
067400020214     C                   movel     'Tutti'       s1dpou
067500020214     C                   endif
067600010926      *
067700010921     C                   ENDSR
067800020207      *--------------------------------------------------------------------------------------------*
067900020207      * AtrS1  - Ripristino giusti attributi su sfl
068000020207      *--------------------------------------------------------------------------------------------*
068100011017     C     AtrS1         BEGSR
068200011017      *
068300020207      * record annullato
068400020207     C     tbeatb        ifeq      'A'
068500020207     C                   movel     'A'           S1CAnn
068600020208     C                   seton                                        43
068700020215     C                   move      *on           s1hflg
068800020207     C                   else
068900020207     C                   movel     ' '           S1CAnn
069000020208     C                   setoff                                       43
069100020215     C                   move      *off          s1hflg
069200020207     C                   endif
069300011017      *
069400020207     C                   ENDSR
069500020208      *--------------------------------------------------------------------------------------------*
069600020208      * DeCod  - Decodifica codice cliente
069700020208      *--------------------------------------------------------------------------------------------*
069800020208     C     DeCod         BEGSR
069900020208      *
070000020208     C                   clear                   tibs69ds
070100120207     C                   z-add     DUTkci        I69Kcc
070200020208     C                   movel     $Cod          I69Kac
070300020208     C                   movel     KnSif         I69Sif
070400020208     C                   call      'TIBS69R'
070500020208     C                   parm                    Tibs69Ds
070600020208     C                   parm                    DsAco
070700020208     C                   parm                    DsInd
070800020208     C                   parm                    DsClp
070900020208     C                   parm                    DsCls
071000020208      *
071100020208     C                   ENDSR
071200020208      *--------------------------------------------------------------------------------------------*
071300020208      * CtrSearch - Controlli prima delle ricerche
071400020208      *--------------------------------------------------------------------------------------------*
071500020208     C     Ctrsearch     BEGSR
071600020208     C                   setoff                                       5556
071700020208      *
071800020208      * Ricerca solo se P.O a zero o codice valido
071900020208     C     '?'           scan      V1CCod                                 55
072000020208     C     *in55         ifeq      *on
072100020208     C     v1cpou        andne     *zeros
072200020208      *
072300020208      * segnalo che ho richiesto la ricerca
072400020208     C                   seton                                        56
072500020208     C     v1cpou        chain     Azorg01l                           21
072600020208     C   21              setoff                                       55
072700020208     C                   endif
072800020208      *
072900020208     C                   ENDSR
073000020207      *--------------------------------------------------------------------------------------------*
073100020207      * RutInz - operazioni iniziali
073200020207      *--------------------------------------------------------------------------------------------*
073300000210     C     rutinz        BEGSR
073400020207      *
073500020207      * Ricevimento parametri
073600000000     C     *ENTRY        PLIST
073700000000     C                   PARM                    kpjba
073800110805     c                   parm                    TNTA61ds
073900020207      *
074000120207     C                   z-add     1             CodUt             1 0
074100120207      /free
074200120207
074300120207         // -?Impostazione nome programma a video?
074400120207         V1Tpgm = SDSpgm;
074500120207
074600120207         // -?Reperimento Dati del job (Utente/Operativi).?
074700120207         in(E) §AzUte;
074800120207         if NOT %error;
074900120207           in(E) §DatiUte;
075000120207         endif;
075100120207         if %error or RSut = *blanks;
075200120207           clear TIBS34ds;
075300120207           tibs34r ( tibs34ds );
075400120207           in §AzUte;
075500120207           in §DatiUte;
075600120207         endif;
075700120207
075800120207      /end-free
075900020207      *
076000020207      * Recupero S.I.
076100020207      *
076200020207     C                   MOVEL     *blanks       tbecod                         *tabella
076300020207     C                   MOVEL     *all'0'       tbeke1                         *chiave uno
076400020207     C                   MOVE      '3NN'         tbeke1
076500020207     C     k01tbe2       chain     tntbe01l                           97
076600020207     C  N97              movel     tbesif        $sif             10
076700120207     C   97              move      *on           $Fine
076800020207      *
076900020207      *
077000020207      * chiavi di lettura
077100020207     C     K01tbe2       KLIST                                                  *tntbe01l
077200020207     C                   KFLD                    tbecod                          -tabella
077300020207     C                   KFLD                    tbeke1                          -chiave uno
077400020207      *
077500020207     C     K01tbe5       KLIST                                                  *tntbe01l
077600020207     C                   KFLD                    tbecod                          -tabella
077700020207     C                   KFLD                    tbeke1                          -chiave uno
077800020207     C                   KFLD                    tbeke2                          -chiave due
077900020207     C                   KFLD                    tbelin                          -lingua
078000020207     C                   KFLD                    tbesif                          -s.informativo
078100010921      *
078200020207     C     k02tbe4       KLIST                                                  *tntbe01l
078300020207     C                   KFLD                    tbecod                          -tabella
078400020207     C                   KFLD                    tbelin                          -chiave uno
078500020207     C                   KFLD                    tbesif                          -chiave due
078600020207     C                   KFLD                    tbeke1                          -lingua
078700010924      *
078800020207     C     k02tbe3       KLIST                                                  *tntbe01l
078900020207     C                   KFLD                    tbecod                          -tabella
079000020207     C                   KFLD                    tbelin                          -chiave uno
079100020207     C                   KFLD                    tbesif                          -chiave due
079200010924      *
079300020207     C                   ENDSR
079400020207      *--------------------------------------------------------------
079500010927** MSG  Lungh. 78                                                            *
079600020212Opzione non valida
079700020212Impossibile ripristinare un record già valido
079800020212Record annullato
