000100120712     /*PRM dbgview(*source)
000200120712     /*CMD ovrdbf file(FIAR531C) tofile(GAITRAGRPS/FIAR531C) +
000300120712     /*CMD        ovrscope(*calllvl)
000400120712     /*END dltovr file(FIAR531C) lvl(*)
000500120712     /*END
000600040308     H DECEDIT('0,') DATEDIT(*YMD.)
000700120710     h option(*nodebugio)
000800120710     h dftactgrp(*no)
000900120710     h bnddir('UBRTVNETA')
001000120710      *---------*
001100120710      * FNLR80R *----------------------------------------------------*
001200120710      *---------*                                                    *
001300120710      *         - ELENCO BOLLE CON OBBLIGO RESTITUZIONE -            *
001400120710      *            - BAM FIRMATA/LETTERA DI VETTURA -                *
001500120710      *--------------------------------------------------------------*
001600000221     FFNARBX1L  UF   E           K DISK    USROPN
001700960117     F*---------
001800000221     FFNARBX3L  UF   E           K DISK    USROPN
001900000221     F                                     RENAME(FNARBX00:FNARBX3)
002000000221     F*---------
002100960117     FFNARB01L  IF   E           K DISK
002200960117     F*---------
002300040316     FFnLBL01L  IF   E           K DISK
002400040316     F*---------
002500040316     FFnart01L  IF   E           K DISK
002600040303     F*---------
002700050309     Ftigcp21L  IF   E           K DISK
002800040303     F*---------
002900040303     FFiar501L  IF   E           K DISK
003000120712     f*---------
003100120712     fFIAR531C  IF   E           K DISK    usropn
003200120712     f                                     extfile(wLibFIAR5)
003300120712     f                                     rename(FIAR5000:FIAR500S)
003400120712     f                                     rename(FIAR5P00:FIAR5P0S)
003500040303     F*---------
003600960117     FTABEL00F  IF   E           K DISK
003700120710     f*---------
003800120710     fTNTBE01L  if   e           k disk    usropn
003900120710     f                                     extfile(wLibTNTBE)
004000960117     F*---------
004100960117     FAZORG01L  IF   F 5000     2PIDISK    KEYLOC(4)
004200960117     F*---------
004300991130     FFNLR80P   O    E             PRINTER OFLIND(*IN04)
004400120625      *--------------------------------------------------------------*
004500120625      *   c o s t a n t i                                            *
004600120625      *--------------------------------------------------------------*
004700120625     D CPERDP          C                   CONST('   DA INVIARE   IN -
004800120625     D                                        SEDE')
004900120625     D CNOBOL          C                   CONST('NESSUNA BOLLA')
005000120710
005100120710       //--------------------------------------------------------------
005200120710       //?Definizione aree dati.                                       ?
005300120710       //--------------------------------------------------------------
005400120710
005500120710       // -?Dati utente?
005600120710     d §AzUte        e ds                  extname(AZUTE00F)
005700120710     d                                     dtaara
005800120710     d §DatiUte      e ds                  extname(dDatiUte)
005900120710     d                                     dtaara
006000960117      *--------------------------------------------------------------*
006100960117      *   s c h i e r e                                              *
006200960117      *--------------------------------------------------------------*
006300960117     D C7R             S              2    DIM(100)                             SKI CHIAVI 7R
006400960117     D T7R             S              1    DIM(100)                             STP.BAM/L.VETT
006500040319     D C7Q             S              2    DIM(100)                             SKI CHIAVI 7R
006600040319     D D7Q             S             89    DIM(100)                             STP.BAM/L.VETT
006700040319     D D7W             S             89    DIM(100)                             STP.BAM/L.VETT
006800120625     d $3A             S                   dim(100) like(ARBcbo)
006900120625      *--------------------------------------------------------------*
007000120625      *   s t r u t t u r e   d a t i                                *
007100120625      *--------------------------------------------------------------*
007200120706     D FNLR80DS      E DS
007300120709     D DAR5DOC       E DS                  inz   qualified
007400040303     D DAR5GEN       E DS
007500120625     d ds3A          e ds                  inz
007600040319     D Ds7w          E DS
007700040319     D Ds7q          E DS
007800120625     D DS7R          E DS
007900120625      * -?Tab. VPO = Variabili Programmi Operativi?
008000120625     d dVPO          e ds                  inz
008100120625      * -?Tab. JDC = Clienti x ritorno immagini DOC?
008200120625     d dJDC          e ds                  inz
008300120625      *
008400960117     D WLBDA8          DS
008500960117     D  G02DAT                 1      8  0
008600960117     D  G02INV                 9     16  0
008700960117     D  G02ERR                17     17
008800960117     D  G02TGI                18     22  0
008900040304      *
009000040304      * Stampa dettaglio colli non sequenziali
009100040304     D  dsNCL          DS
009200040304     d stcnc0                  1      7  0
009300040304     d stcnc1                  8     14  0
009400040304     d stcnc2                 15     21  0
009500040304     d stcnc3                 22     28  0
009600040304     d stcnc4                 29     35  0
009700040304     d stcnc5                 36     42  0
009800040304     d stcnc6                 43     49  0
009900040304     d stcnc7                 50     56  0
010000040304     d stcnc8                 57     63  0
010100040304     d stcnc9                 64     70  0
010200040304     d stcncA                 71     77  0
010300040304     d stcncB                 78     84  0
010400040304     d stcncC                 85     91  0
010500040304     d stcncD                 92     98  0
010600040304     d stcncE                 99    105  0
010700040304     D    NCL                  1    105  0 DIM(15)
010800040304      *
010900040304      * Data attivazione scan POD - LDV EUROEXPRESS
011000040316     D OG146         E DS                  prefix(§146_)
011100040316     D OG143         E DS                  prefix(§143_)
011200040304      *
011300960117     D KPJBA         E DS
011400120625      *
011500960117     D                SDS
011600960117     D  NOMPGM                 1     10
011700120625      *--------------------------------------------------------------*
011800120625      *   v a r i a b i l i                                          *
011900120625      *--------------------------------------------------------------*
012000120712     d $CallTNSB25     s               n   inz
012100120911     d*// $in07parz       s               n   inz
012200120625     d wCount          s              5  0 inz
012300120710     d Udate8          s              8  0
012400120710
012500120710       // -?Nome esteso Libreria/File del TNTBE01L di Sede?
012600120710     d wLibTNTBE       s             21a   inz
012700120712       // -?Nome esteso Libreria/File del FIAR531C di Sede?
012800120712     d wLibFIAR5       s             21a   inz
012900120625
013000120625       //--------------------------------------------------------------
013100120625       //?Definizione prototipi procedure.                             ?
013200120625       //--------------------------------------------------------------
013300120710
013400120710       // -?Nome del sistema?
013500120710     d currSysNeta     s              8a   inz
013600120710       // -?Reperimento NETA sistema AS/400 corrente?
013700120710      /copy gaitrasrc/srcProtoPR,UBRTVNETA
013800120625
013900120710       // -?Reperimento dati utente?
014000120710     d TIBS34ds      e ds
014100120710      /copy gaitrasrc/srcProtoPR,TIBS34R
014200120710
014300120625       // -?Ricerca/controllo tabelle?
014400120625     d TIBS02ds      e ds                  inz
014500120625      /copy gaitrasrc/srcProtoPR,TIBS02R
014600120625
014700120625       // -?Stampa copia LdV x Scannerizzazione cocumenti clienti?
014800120625     d TNSB25ds      e ds                  inz
014900120625     d   D25ges      e                     inz('T')
015000120625      /copy gaitrasrc/srcProtoPR,TNSB25R
015100120710
015200120710       //--------------------------------------------------------------
015300120710       //?Definizione key-list.                                        ?
015400120710       //--------------------------------------------------------------
015500120710
015600120710       // -?File TNTBE01L?
015700120710     d k05tntbe01    e ds                  extname(TNTBE01L : *key)
015800120710     d                                     prefix(k_)   inz
015900120625
016000960117      *--------------------------------------------------------------*
016100960117      *   D s                                                        *
016200960117      *--------------------------------------------------------------*
016300960117     IAZORG01L  AA
016400960117     I                             P    4    5 0ORGFIL
016500960117     I                                 14   33  ORGDES
016600040305     I                               3776 3800  ORGDE3
016700040305     I                               3851 3875  ORGDE6
016800960117      *--------------------------------------------------------------*
016900960117      *   C I C L O    P R I N C I P A L E                           *
017000960117      *--------------------------------------------------------------*
017100120710     C                   SETON                                        LR
017200960117      *  Leggo record con data di stampa bolla uguale a quella rich.
017300000221      *  ... se DPD ordinate x numero distinta
017400000221     C     D80DPD        IFEQ      'S'
017500120709     C                   exsr      sr_ElabDPD
017600000221      *  ... se non DPD ordinate x numero spedizione
017700000221     C                   ELSE
017800120709     C                   exsr      sr_ElabBRT
017900000221     C                   END
018000960117      *
018100960117      *  Se sono in stampa richiamo pgm. pulizia
018200960117     C     D80RIS        IFNE      'S'
018300960117     C                   CALL      'FNLR80R1'
018400960117     C                   PARM                    KPJBA
018500960117     C                   END
018600960117      *
018700960117     C     FINE          TAG
018800120710     C                   return
018900120709      *--------------------------------------------------------------*
019000120709      *  sr_ElabDPD: Elaborazione x DPD                              *
019100120709      *--------------------------------------------------------------*
019200120709     C     sr_ElabDPD    BEGSR
019300120709      *
019400120709      *  Leggo record con data di stampa bolla uguale a quella rich.
019500120709      *  ... se DPD ordinate x numero distinta
019600120709     C                   Z-ADD     D80FGS        KFGS
019700120709     C                   Z-ADD     D80DST        KDST
019800120709     C     KARBX         CHAIN     FNARBX3L                           30
019900120709     C     *IN30         DOWEQ     '0'
020000120709      * testa tipologie valide
020100120709     c                   if        arbtpl = ' ' or arbtpl = 'C'
020200120709      *
020300120709      *  Attenzione: per le filiali che hanno iniziato la scannerizzazione
020400120709      *   dei POD non si deve stampare nulla e cancellare i records dal file
020500120709      *    spia ARBX....         Data inizio scannerizzazione
020600120709     c                   move      'S'           stampaE           1
020700120709     c                   move      *zeros        data_attiv        8 0
020800120709     c                   move      *zeros        lblLPO
020900120709      *
021000120709     C                   IF        §146_§ogDSD <> *blank or
021100120709     C                             §146_§ogDSE <> *blank
021200120709      *
021300120709     C     ArbLNP        CHAIN     AZORG01L                           40
021400120709      *
021500120709     c     *in40         ifeq      *off
021600120709     C                   MOVEL     ORGDE3        OG143
021700120709      *
021800120709      * Attenzione: se bolla non estero e dirottata occorre controllare la
021900120709      *  bolla originale
022000120709      *   x vedere se non deve essere stampata la figlia....
022100120709     c                   if        §143_§OGNTW <> 'EEX' and
022200120709     c                             §143_§OGNTW <> 'DPD'
022300120709      * file legami con bolla originale
022400120709     C     KLBL          chain     fnLBL000
022500120709     c                   if        %Found(fnLBL01L) and LBLLPO <> arblnp
022600120709      * con la bolla originale si controlla se linea estera
022700120709     C     LBLlpo        CHAIN     AZORG01L
022800120709     c                   if        %Found(azorg01l)
022900120709     C                   MOVEL     ORGDE3        OG143
023000120709     c                   end
023100120709     c                   end
023200120709     c                   end
023300120709      *
023400120709      * Euroexpress
023500120709     c     §143_§ogNTW   ifeq      'EEX'
023600120709     c     §146_§ogDSE   andne     *blank
023700120709     c     §146_§ogDSE   andne     *all'0'
023800120709     c                   move      §146_§ogDSE   data_attiv        8 0
023900120709     c                   end
024000120709      *
024100120709      * Chiodo per Timberland le cui spedizioni hanno LNP = 89 ma fa parte
024200120709      * del circuito Euroexpress
024300120709     c     §146_§ogDSE   ifne      *blank
024400120709     c     §146_§ogDSE   andne     *all'0'
024500120709     c     arbLNP        ifeq      089
024600120709     c     lblLPO        oreq      089
024700120709     c                   move      §146_§ogDSE   data_attiv        8 0
024800120709     c                   end
024900120709     c                   end
025000120709      * Dpd
025100120709     c     §143_§ogNTW   ifeq      'DPD'
025200120709     c     §146_§ogDSD   andne     *blank
025300120709     c     §146_§ogDSD   andne     *all'0'
025400120709     c                   move      §146_§ogDSD   data_attiv
025500120709     c                   end
025600120709      *
025700120709      * Se la data di attivazione è attiva non deve stampare
025800120709     c     data_Attiv    ifle      udate8
025900120709     c     data_Attiv    andne     *zeros
026000120709     c                   move      'N'           stampaE           1
026100120709     c                   end
026200120709      *
026300120709     c                   end
026400120709     c                   endIF
026500120709      *
026600120709      * Solo se non è stato disabilitato
026700120709     c                   if        StampaE ='S'
026800120709      *  Stampo dati bolla
026900120709     C                   EXSR      STPBOL
027000120709     c                   endIF
027100120709      *endif tipologie
027200120709     c                   endif
027300120709      *  Lettura successiva
027400120709     C     KARBX         READE     FNARBX3L                               30
027500120709     C                   END
027600120709     C* STAMPA TOTALI
027700120709     C     STTOTB        IFEQ      0
027800120709     C                   WRITE     TES01
027900120709     c                   write     Tes02
028000120709     c                   write     Tes04
028100120709     C                   MOVEL     CNOBOL        STMSG1
028200120709     C                   ENDIF
028300120709     C                   WRITE     TOT01
028400120709      *
028500120709     C                   ENDSR
028600120709      *--------------------------------------------------------------*
028700120709      *  sr_ElabDPD: Elaborazione x BRT                              *
028800120709      *--------------------------------------------------------------*
028900120709     C     sr_ElabBRT    BEGSR
029000120709      *
029100120709      *  Leggo record con data di stampa bolla uguale a quella rich.
029200120709      *  ... se non DPD ordinate x numero spedizione
029300120709     C                   Z-ADD     D80FGS        KFGS
029400120709     C                   Z-ADD     D80DST        KDST
029500120709     C     KARBX         CHAIN     FNARBX1L                           30
029600120709     C     *IN30         DOWEQ     '0'
029700120709      * testa tipologie valide
029800120709     c                   if        arbtpl = ' ' or arbtpl = 'C'
029900120709      *
030000120709      *  Stampo dati bolla
030100120709      *      SE MERCE DA DISTRUGGERE
030200120709     C     D80DPD        IFEQ      'X'
030300120709     C     ARBTPL        ANDEQ     'C'
030400120709     C                   EXSR      STPBOL
030500120709     C                   ENDIF
030600120709      *
030700120709      *      SE BOLLE FIRMATE
030800120709     C     D80DPD        IFNE      'X'
030900120709     C     ARBTPL        ANDEQ     ' '
031000120709     C                   EXSR      STPBOL
031100120709     C                   ENDIF
031200120709      * endif tipologie
031300120709     c                   endif
031400120709      *  Lettura successiva
031500120709     C     KARBX         READE     FNARBX1L                               30
031600120709     C                   END
031700120709      *
031800120709     C                   ENDSR
031900960117      *--------------------------------------------------------------*
032000120709      *  STPBOL: Stampa bolla (filtro)                               *
032100960117      *--------------------------------------------------------------*
032200960117     C     STPBOL        BEGSR
032300960117      *
032400040305      *  Esegue rotture di livello solo se non si tratta di merce da
032500040305      *    distruggere.
032600040305     c                   if        arbtpl <> 'C'
032700991222      *  Eseguo posizionamento solo se bolla  è cambiata
032800960118     C  N05ARBLNP        COMP      WSVLNP                             0505
032900000225     C  N06ARBDDC        COMP      WSVDDC                             0606
033000120706     c                   endif
033100040305      *
033200000225     C                   Z-ADD     ARBDDC        WSVDDC            8 0
033300000225      *  Giro la data distinta di consengna
033400000225     C                   Z-ADD     0             STCDDC            8 0
033500000225     C     ARBDDC        IFNE      0
033600000225     C                   Z-ADD     ARBDDC        G02INV
033700000225     C                   MOVEL     '3'           G02ERR
033800000225     C                   CALL      'XSRDA8'
033900000225     C                   PARM                    WLBDA8
034000000225     C                   Z-ADD     G02DAT        STCDDC            8 0
034100000225     C                   Z-ADD     ARBNDC        STCNDC
034200120706     C                   ENDIF
034300991222      *  ABBLENCO FLAG DI STAMPA
034400991222     C                   CLEAR                   WSTAMP
034500991222      *
034600960117     C                   Z-ADD     ARBAAS        WSVAAS
034700960117     C                   Z-ADD     ARBLNP        WSVLNP
034800960117     C                   Z-ADD     ARBNRS        WSVNRS
034900960117     C                   Z-ADD     ARBNSP        WSVNSP
035000120706      *
035100120706      *  Decodifico la Linea di Partenza a rottura della stessa
035200120706     C                   if        *in05
035300120706     C                   Z-ADD     ARBLNP        STCLNP
035400120709     C     ARBlnp        CHAIN     AZORG01L
035500120706     C                   if        %found(AZORG01L)
035600120706     C                   MOVEL     ORGdes        STDlnp
035700120706     C                   else
035800120706     C                   clear                   STDlnp
035900120706     C                   endif
036000120911      *//* - Verifica se anche la LNP è abilitata alla scannerizzazione
036100120911      *//*   (in tab. "WS")
036200120911     C*//                select
036300120911     C*//                when      not  $in07parz
036400120911     C*//                eval      *in07 = $in07parz
036500120911     C*//                when      $in07parz  and  D80sns = 'S'
036600120911     C*//                eval      *in07 = $in07parz
036700120911     C*//                when      $in07parz  and  D80sns = 'N'
036800120911     C*//                eval      Kkut = 1
036900120911     C*//                eval      Kcod = 'WS'
037000120911     C*//                eval      Kkey = %editc( ARBlnp : 'X' )
037100120911     C*//  KTAB1         chain     TABEL
037200120911     C*//                eval      *in07 = (%found(TABEL00F)  and
037300120911     C*//                                   TBLflg = *blank)
037400120911     C*//                endsl
037500120911      * - Considera ora TUTTE le filiali abilitate alla scannerizzazione
037600120911     c                   eval      *in07 = *on
037700120706     C                   endif
037800120706      *
037900960117      *  Mi posiziono sull'archivio delle bolle in arrivo per
038000960117      *  effettuare stampa
038100960117     C                   Z-ADD     ARBAAS        KAAS
038200960117     C                   Z-ADD     ARBLNP        KLNP
038300960117     C                   Z-ADD     ARBNRS        KNRS
038400960117     C                   Z-ADD     ARBNSP        KNSP
038500120625     C     KARB          CHAIN     FNARB000                           31
038600120625if  1C     *IN31         IFEQ      '0'
038700960117      *--------------------------------------------------------------*
038800960117      *  Se ristampa considero bolla sempre
038900120625if  2C     D80RIS        IFEQ      'S'
039000991207      *  stampa solo se è stata CONSEGNATA
039100991207     C     ARBDCM        ORNE      0
039200040303      *
039300040319      *  Se deve fare elenco merce da distruggere
039400120625if  3c                   if        *in99 = *on
039500040319      *  98 on  = GD
039600040319      *  98 off = GS
039700040319     c                   seton                                        98
039800040319      *
039900040319     C                   MOVEL     ARBGgA        STCGMA
040000040319     C                   Z-ADD     1             X
040100040319     C     arbGGA        LOOKUP    C7Q(X)                                 32
040200120625if  4c                   if        *in32 = *on
040300040319     c                   movel     d7W(X)        ds7w
040400120625if  5c                   if        §7W7CA1 = 'S' or
040500040319     c                             §7W7CA2 = 'S' or
040600040319     c                             §7W7CA3 = 'S'
040700040319      * variabilizzato il chiodo "GD"
040800040319     c                   setoff                                       98
040900120706e   5c                   endif
041000040319      *
041100040319      *  record da non utilizzare x distruzione
041200040319     c                   setoff                                       32
041300040319      * se da distruggere
041400120625if  5c                   if        §7WCA1 = 'S' or
041500040319     c                             §7WCA2 = 'S' or
041600040319     c                             §7WCA3 = 'S' or
041700040319     c                             §7WCA4 = 'S' or
041800040319     c                             §7WCA5 = 'S' or
041900040319     c                             §7W7CA1 = 'S' or
042000040319     c                             §7W7CA2 = 'S' or
042100040319     c                             §7W7CA3 = 'S'
042200040319     c                   seton                                        32
042300120706e   5c                   endif
042400120706e   4c                   endif
042500040319      *
042600120625x   3c                   else
042700120706      *
042800120706      * Se Lista invio alla Partenza
042900040319     C                   Z-ADD     1             X
043000040319     C     ARBGMA        LOOKUP    C7R(X)                                 32
043100120709      *
043200120713      *
043300120727     c                   eval      $CallTNSB25 = *off
043400120727      *
043500120727      * - Se FGS abilitata alla scannerizzazione:
043600120727      *   verifica se cliente abilitato e se documenti già scannerizzati
043700120911      *  ?SE NON "Elenco sped. con obbligo restit. doc. alla LNP"?
043800120911      *     ?(vedi *in07)?
043900120911      *  ?=> NON va stampata l'indicazione "SCANNERIZZATI"/"DA SCANNERIZZ."?
044000120911      *     ?(quindi inutile controllare l'abilitazione del mittente)?
044100120727if  4c                   if        *in32  and  *in07
044200120727      *
044300120713      * - Reperimento tab. "JDC" del cliente
044400120713     c                   exsr      sr_RepTabJDC
044500120727      * - Verifica se documenti da_scannerizzare/scannerizzati
044600120713     c                   clear                   dAR5doc
044700120713if  5C                   if        D80sns      = 'N'   or
044800120713     C                             $CallTNSB25 = *on
044900120709     C                   eval      KGEN = 'DOC'
045000120712     C     K501          chain     FIAR531C
045100120713if  6c                   if        %found(FIAR531C)
045200120709     c                   movel     AR5uni        dAR5doc
045300120713e   6c                   endif
045400120713e   5C                   endif
045500120727      * - Se richieste (in ristampa) le sole spedizioni con documenti
045600120712      *   NON ancora scannerizzati: seleziono solo quelli da scannerizzare
045700120713if  5C                   if        D80sns       = 'S'   and
045800120712     C                             ($CallTNSB25 = *off   or
045900120712     C                              dAR5doc.§AR5jfid = 'S')
046000120709     C                   eval      *in32 = *off
046100120709e   5C                   endif
046200120713      *
046300120709e   4C                   endif
046400120709      *
046500120706e   3c                   endif
046600120727      *
046700991130     C*
046800120625if  3C     *IN32         IFEQ      *ON
046900991130     C     D80DPD        ANDEQ     'N'
047000991130     C     T7R(X)        ANDNE     'D'
047100991130     C     *IN32         OREQ      *ON
047200991130     C     D80DPD        ANDEQ     'S'
047300991130     C     T7R(X)        ANDEQ     'D'
047400120706     C     *IN32         OREQ      *ON
047500021029     C     D80DPD        ANDEQ     'X'
047600120706     C                   exsr      StpBol2
047700120709e   3C                   ENDIF
047800120712if  3c                   if        *in32          and
047900120712     c                             D80dpd =  'N'  and
048000120712     c                             T7R(X) <> 'D'  and
048100120712     c                             WSTAMP =  'S'  and
048200120712     c                             $CallTNSB25      = *on   and
048300120712     c                             dAR5doc.§AR5jfid = *blank
048400120709     c                   exsr      sr_CallTNSB25
048500120712e   3c                   endif
048600960117      *
048700120706e   2C                   ENDIF                                                  bolla OK
048800120706e   1C                   ENDIF                                                  trova bolla
048900960117      *
049000991207      *  Se sono in stampa aggiorno FILE SPIA
049100120625if  1C     D80RIS        IFNE      'S'
049200960117     C     *IN31         ANDEQ     '0'
049300991222      *  Aggiorna solo se è stata STAMPATA
049400120625if  2C     WSTAMP        IFEQ      'S'
049500960117     C                   Z-ADD     WOGGI         ARBDST
049600120625if  3C     D80DPD        IFEQ      'S'
049700000221     C                   EXCEPT    AGGDA3
049800120625x   3C                   ELSE
049900000221     C                   EXCEPT    AGGDAT
050000120706e   3C                   ENDIF                                                  D.P.D
050100120706e   2C                   ENDIF                                                  BOLLA OK
050200120706e   1C                   ENDIF                                                  stampa
050300960117      *
050400960117     C                   ENDSR
050500120706      *--------------------------------------------------------------*
050600120709      *  STPBOL2: Stampa bolla in elenco                             *
050700120706      *--------------------------------------------------------------*
050800120706     C     STPBOL2       BEGSR
050900120706      *
051000120706      *  Giro la data distinta di consengna in base alla bolla
051100120706      *  solo se non l'ho già impostata
051200120706if  1C     STCDDC        IFEQ      0
051300120706     C                   Z-ADD     ARBDDC        G02INV
051400120706     C                   MOVEL     '3'           G02ERR
051500120706     C                   CALL      'XSRDA8'
051600120706     C                   PARM                    WLBDA8
051700120706     C                   Z-ADD     G02DAT        STCDDC            8 0
051800120706     C                   Z-ADD     ARBNDC        STCNDC
051900120706e   1C                   ENDIF
052000120706      *  Giro la data spedizione
052100120706     C                   MOVEL     ARBMGS        WMM               2 0
052200120706     C                   MOVE      ARBMGS        WGG               2 0
052300120706     C                   MOVE      WMM           STCMGS
052400120706     C                   MOVEL     WGG           STCMGS
052500120706      *  Giro i dati
052600120706     C                   Z-ADD     ARBLNA        STCLNA
052700120706     C                   Z-ADD     ARBAAS        STCAAS
052800120706     C*//                Z-ADD     ARBLNP        STCLNP
052900120706     C                   Z-ADD     ARBNRS        STCNRS
053000120706     C                   Z-ADD     ARBNSP        STCNSP
053100120706     C                   Z-ADD     ARBRMN        STCRMN
053200120706      *  Deve segnalare la Zona di consegna al posto del cod.Bolla
053300120706     C                   MOVEL     ARBZNC        STCZNC
053400120706     C                   MOVEL     ARBRSM        STCRSM
053500120706     C                   MOVEL     ARBRSD        STCRSD
053600120706     C                   MOVEL     ARBRMA        STCRMA
053700120706     C                   MOVEL     ARBGMA        STCGMA
053800120706      *  Impostazione indicazione della scannerizzazione
053900120712     c                   clear                   STscann
054000120727     c                   eval      *in09 = *off
054100120712if  1c                   if        $CallTNSB25
054200120712if  2C                   if        dAR5doc.§AR5jfid = *blank
054300120709     C                   eval      STscann = 'DA SCANNERIZZ. '
054400120727     c                   eval      *in09 = *on
054500120712x   2C                   else
054600120713     C                   eval      STscann = 'SCANNERIZZATI  '
054700120712e   2C                   endif
054800120712e   1c                   endif
054900120706      *
055000120706     C                   SETOFF                                       0203
055100120706if  1C     T7R(X)        IFEQ      'B'
055200120706     C                   SETON                                            02
055300120706x   1C                   ELSE
055400120706     C                   SETON                                            03
055500120706e   1C                   ENDIF
055600120706     C* Se si è rotta la chiave
055700120706     C* e la stampa non è per DPD
055800120709     C* e NON è stata richiesta la RIstampa delle sole spedizioni
055900120709     C*   con documenti da restituire e NON ancora scannerizzati
056000120706     C* salto pagina.
056100120706if  1C     *IN05         IFEQ      *ON
056200120706     C     D80DPD        ANDEQ     'N'
056300120709     C     D80sns        andne     'S'
056400120706     C     *IN05         orEQ      *ON
056500120706     C     D80DPD        ANDEQ     'X'
056600120706     C                   SETON                                        04
056700120706e   1C                   ENDIF
056800120706     C* Se si è rotta la data distinta x bolle DPD
056900120706     C* salto pagina.
057000120706if  1C     *IN06         IFEQ      *ON
057100120706     C     D80DPD        ANDEQ     'S'
057200120706if  2C     STTOTB        IFNE      0
057300120706     C                   WRITE     TOT01
057400120706e   2C                   ENDIF
057500120706     C                   SETON                                        04
057600120706e   1C                   ENDIF
057700120706      *
057800120706     C* Salto pagina.
057900120706     c                   exsr      Se_OVERFLW
058000120706      *
058100120706     C                   WRITE     DET01
058200120706     C                   ADD       1             STTOTB
058300120706      *
058400120706      * Se I.M.G. quindi con codice 'GS' 98=(off) deve stampare su altra riga
058500120706      *    le informazioni di giacenza e ubicazione.
058600120706if  1c                   IF        *in99 = *on
058700120706      *
058800120706     c                   clear                   stcubi
058900120706     c                   clear                   stcfgc
059000120706     c                   clear                   stcngc
059100120706      *
059200120706      * Solo x I.M.G. (Giacenza e ubicazione)
059300120706if  2c                   if        *in98 = *off
059400120706     C     Kgca          CHAIN     tigcp000
059500120706if  3c                   if        %Found(tigcp21L)
059600120706     c                   eval      stcfgc = gcpfgc
059700120706     c                   eval      stcngc = gcpngc
059800120706      * reperisce l'ubicazione se c'è della giacenza
059900120706     C                   move      'GEN'         KGEN
060000120706     C     K501          Chain     Fiar5000
060100120706if  4c                   if        %Found(Fiar501L)
060200120706     c                   movel     ar5uni        dar5GEN
060300120706      * Con la DS "GEN" reperisce l'ubicazione
060400120706if  5c                   if        §AR5ZNG <> *blank
060500120706     c                   eval      stcubi = §AR5ZNG
060600120706e   5c                   endif
060700120706e   4c                   endif
060800120706e   3c                   endif
060900120706e   2c                   endif
061000120706      *
061100120706     C* Stampa una seconda riga per indicare i colli da distruggere
061200120706      *    dettaglio segnacolli
061300120706     C                   clear                   NCL
061400120706     c                   z-add     0             NC                3 0
061500120706     c                   seton                                        34
061600120706      *
061700120706     C     KARB          setll     Fnart000
061800120706     C     KARB          reade     Fnart000                               35
061900120706      *
062000120706      * Se per qualche strano motivo non ci fosse ART deve comunque stampare
062100120706      *  la giacenza e ubicazione (NON ACCADRA' MAI)
062200120706if  2c                   if        %EoF(Fnart01L)
062300120706     c                   exsr      Se_OVERFLW
062400120706     C                   WRITE     DETcol
062500120706e   2c                   endif
062600120706      *
062700120706do  2c                   dow       not %EoF(Fnart01L)
062800120706      *
062900120706      * Memorizza un segnacollo in una posizione di schiera
063000120706     c                   add       1             NC
063100120706     c                   z-add     artNSC        NCL(nc)
063200120706     C     KARB          reade     Fnart000                               35
063300120706      *
063400120706      *  Se è arrivato al limite di schiera (della riga da stampare)
063500120706      *  o se si trova con dei segnacollli impostati da stampare ma è
063600120706      *  arrivato a EoF quindi uscirebbe dal ciclo --> EMETTE LA RIGA.
063700120706if  3c                   if        nc = 15 or %EoF(Fnart01L)
063800120706     c                   exsr      Se_OVERFLW
063900120706     C                   WRITE     DETcol
064000120706     c                   setoFF                                       34
064100120706     C                   clear                   NCL
064200120706     c                   z-add     0             NC
064300120706e   3c                   endif
064400120706      *
064500120706e   2c                   enddo
064600120706      *
064700120706e   1c                   ENDIF
064800120706      *
064900120706     C                   MOVEL     'S'           WSTAMP            1            SI STAMPA
065000120706      *
065100120706     C                   ENDSR
065200120706      *
065300960117      *--------------------------------------------------------------*
065400040303      *  Salto Pagina imposta la testata                             *
065500120709      *  Rottura LNP in RIstampa spedizioni con restituzione docum.  *
065600120709      *    alla Partenza: stampa intestazione LNP                    *
065700960117      *--------------------------------------------------------------*
065800040303     C     Se_OVERFLW    BEGSR
065900040303      *
066000040303     C* Salto pagina.
066100040303     C     *IN04         IFEQ      *ON
066200120709      *
066300040303     C                   SETOFF                                       04
066400040303     C                   WRITE     TES01
066500120709     C                   WRITE     TES02
066600040303      *
066700040303     C* Se si è rotta la chiave  e la stampa non è per DPD
066800120709     C*   e  non è una RIstampa delle spedizioni con documenti da
066900120709     C*      restituire alla partenza NON ancora scannerizzati
067000040303     C* stampo i dati di testata.
067100120730     c                   if        D80ris = 'N'  or  D80sns = 'N'
067200120709     c                   write     Tes03
067300120709     c                   endif
067400120709     c                   write     Tes04
067500120730     c                   if        D80ris = 'S'  and
067600120730     c                             D80sns = 'S'  and
067700120709     c                             D80dpd = 'N'  and
067800120709     c                             *in05  = *on
067900120709     c                   write     Tes03
068000120709     c                   endif
068100120709      *
068200040303     C   05              IF        D80DPD = 'N' or D80DPD = 'X'
068300040303     C                   SETOFF                                       05
068400040303     C                   ENDIF
068500040303     C   06              IF        D80DPD = 'S'
068600040303     C                   SETOFF                                       06
068700040303     C                   Z-ADD     0             STTOTB
068800040303     C                   ENDIF
068900120709      *
069000040303     C                   ENDIF
069100120709      *
069200120709      * Sola intestazione LNP
069300120709     c                   if        *in05  = *on  and
069400120709     c                             D80dpd = 'N'
069500120709     c                   eval      *in05  = *off
069600120730     c                   if        D80ris = 'S'  and  D80sns = 'S'
069700120709     c                   write     TES03
069800120709     c                   else
069900120709     c                   write     TES01
070000120709     c                   write     TES02
070100120709     c                   write     TES03
070200120709     c                   write     TES04
070300120709     c                   endif
070400120709     c                   endif
070500040303      *
070600040303     C                   ENDSR
070700120625      /free
070800120625
070900120625       //-------------------------------------------------------------
071000120625       //?Stampa copia LdV x Scannerizzazione Documenti Clienti       ?
071100120625       //-------------------------------------------------------------
071200120712       BEGSR  sr_RepTabJDC;
071300120712
071400120712         $CallTNSB25 = *off;
071500120625
071600120625         // -?Aggancio tabella "JDC" per reperimento data attivazione?
071700120625         //  ?della scannerizzazione documenti cliente?
071800120625         clear  dJDC;
071900120710         clear  k05tntbe01;
072000120710         k_TBEcod = 'JDC';
072100120702         // -?ARBCCM se valorizzato, ARBKSC altrimenti?
072200120702         if  ARBccm <> *zero;
072300120710           k_TBEke1 = %editc( ARBccm : 'X' );
072400120702         else;
072500120710           k_TBEke1 = %editc( ARBksc : 'X' );
072600120702         endif;
072700120710
072800120710         chain  %kds( k05tntbe01 : 3 )  TNTBE000;
072900120710
073000120710         if  %found(TNTBE01L);
073100120710           dJDC = TBEuni;
073200120710         endif;
073300120625
073400120625         // -?Scannerizzazione documenti per il cliente: NON prevista?
073500120625         //  ?o non ancora in vigore o già scaduta?
073600120710         if  NOT  %found(TNTBE01L)                or
073700120710             §JDCdti > (ARBaas * 10000) + ARBmgs  or
073800120625             §JDCdtf < (ARBaas * 10000) + ARBmgs;
073900120625           leavesr;
074000120625         endif;
074100120712
074200120712         $CallTNSB25 = *on;
074300120712
074400120712       ENDSR;
074500120625
074600120712       //-------------------------------------------------------------
074700120712       //?Stampa copia LdV x Scannerizzazione Documenti Clienti       ?
074800120712       //-------------------------------------------------------------
074900120712       BEGSR  sr_CallTNSB25;
075000120712
075100120625         reset  TNSB25ds;
075200120625         //D25ges = 'T'       ?(già così)?
075300120625         D25aas = ARBaas;
075400120625         D25lnp = ARBlnp;
075500120625         D25nrs = ARBnrs;
075600120625         D25nsp = ARBnsp;
075700120709         if  wCount >= §VPOpagLdV;
075800120625           D25fl1 = *on;
075900120625           clear  wCount;
076000120625         endif;
076100120625
076200120625         tnsb25r ( TNSB25ds );
076300120625
076400120625         wCount += 1;
076500120625
076600120625       ENDSR;
076700120625
076800120625      /end-free
076900040303      *--------------------------------------------------------------*
077000040303      *  Operazioni iniziali                                         *
077100040303      *--------------------------------------------------------------*
077200960117     C     *INZSR        BEGSR
077300960117      *
077400960117     C     *ENTRY        PLIST
077500960117     C                   PARM                    KPJBA
077600120706     C                   MOVEL     KPJBU         FNLR80DS
077700120710      *
077800120710      /free
077900120710
078000120710         // -?Verifica del sistema AS/400 corrente?
078100120710         if  UBRTVNETA_Rtv(currSysNeta) <> *zero;
078200120710           *inLR = *on;
078300120710           return;
078400120710         endif;
078500120710
078600120710         // -?Reperimento dati job?
078700120710         in(E) §AzUte;
078800120710         if NOT %error;
078900120710           in(E) §DatiUte;
079000120710         endif;
079100120710         if %error or RSut = *blanks;
079200120710           clear TIBS34ds;
079300120710           tibs34r ( tibs34ds );
079400120710           in §AzUte;
079500120710           in §DatiUte;
079600120710         endif;
079700120710
079800120710         // -?Apertura file TNTBE01L?
079900120710         if  %subst( currSysNeta : 1 : 6 ) = 'SETRAS';
080000120710           wLibTNTBE = 'GAITRAGRU/TNTBE01L';
080100120712           wLibFIAR5 = 'GAITRAGRU/FIAR531C';
080200120710         else;
080300120710           wLibTNTBE = 'GAITRAGRPS/TNTBE01L';
080400120712           wLibFIAR5 = 'GAITRAGRPS/FIAR531C';
080500120710         endif;
080600120710         open  TNTBE01L;
080700120712         open  FIAR531C;
080800120710
080900120710      /end-free
081000040308     C*
081100120710     C* imposta la data del giorno
081200120710     c                   move      *date         udate8
081300991130     C*
081400991130     C     D80DPD        IFEQ      'S'
081500991130     C                   MOVEL     CPERDP        STTITO
081600991130     C                   ENDIF
081700040303      *
081800120709     C                   eval      *in07 = (D80dpd = 'N')
081900120911     C*//                if        *in07
082000120911     C*//                eval      Kkut = 1
082100120911     C*//                eval      Kcod = 'WS'
082200120911     C*//                eval      Kkey = %editc( D80fgs : 'X' )
082300120911     C*//  KTAB1         chain     TABEL
082400120911     C*//                eval      *in07 = (%found(TABEL00F)  and
082500120911     C*//                                   TBLflg = *blank)
082600120911     C*//                endif
082700120911     C*//                eval      $in07parz = *in07
082800120727     C                   eval      *in08 = (*in07  and  D80sns = 'S')
082900021029     C     D80DPD        COMP      'X'                                    99
083000960117      *----------------------------------------------*
083100960117      *  Definizione chiavi di accesso
083200960117     C     KARB          KLIST
083300960117     C                   KFLD                    KAAS
083400960117     C                   KFLD                    KLNP
083500960117     C                   KFLD                    KNRS
083600960117     C                   KFLD                    KNSP
083700040316      *
083800040316     C     KLBL          KLIST
083900040316     C                   KFLD                    ARBAAS
084000040316     C                   KFLD                    ARBLNP
084100040316     C                   KFLD                    ARBNRS
084200040316     C                   KFLD                    ARBNSP
084300040316      *
084400960117     C     KARBX         KLIST
084500960117     C                   KFLD                    KFGS
084600960117     C                   KFLD                    KDST
084700960117     C     KTAB          KLIST
084800960117     C                   KFLD                    KKUT
084900960117     C                   KFLD                    KCOD
085000040319     C     KTAB1         KLIST
085100040319     C                   KFLD                    KKUT
085200040319     C                   KFLD                    KCOD
085300040319     C                   KFLD                    Kkey
085400040303     C     KGCA          KLIST
085500040303     C                   KFLD                    KAAS
085600040303     C                   KFLD                    KLNP
085700040303     C                   KFLD                    KNRS
085800040303     C                   KFLD                    KNSP
085900040303     C                   KFLD                    KZero
086000040303     C                   z-add     0             KZero
086100040304     C     K501          KLIST
086200040303     C                   KFLD                    KAAS
086300040303     C                   KFLD                    KLNP
086400040303     C                   KFLD                    KNRS
086500040303     C                   KFLD                    KNSP
086600040303     C                   KFLD                    KGEN              3
086700040303     C                   move      'GEN'         KGEN
086800960117      *----------------------------------------------*
086900960117      *  Definizione variabili
087000960117     C     *LIKE         DEFINE    ARBAAS        KAAS
087100960117     C     *LIKE         DEFINE    ARBLNP        KLNP
087200960117     C     *LIKE         DEFINE    ARBNRS        KNRS
087300960117     C     *LIKE         DEFINE    ARBNSP        KNSP
087400960117     C     *LIKE         DEFINE    ARBFGS        KFGS
087500960117     C     *LIKE         DEFINE    ARBDST        KDST
087600960117     C     *LIKE         DEFINE    TBLKUT        KKUT
087700960117     C     *LIKE         DEFINE    TBLCOD        KCOD
087800040319     C     *LIKE         DEFINE    TBLkey        Kkey
087900960117     C     *LIKE         DEFINE    ARBAAS        WSVAAS
088000960117     C     *LIKE         DEFINE    ARBLNP        WSVLNP
088100960117     C     *LIKE         DEFINE    ARBNRS        WSVNRS
088200960117     C     *LIKE         DEFINE    ARBNSP        WSVNSP
088300040304     C     *like         define    gcpfrg        KZero
088400960117      *----------------------------------------------*
088500960117     C                   CLEAR                   WLBDA8
088600960117     C     D80RIS        IFNE      'S'
088700960117      *  Calcolo la data del giorno
088800960117     C                   SETON                                        01
088900960117     C                   TIME                    WHHDAT           14 0
089000960117     C                   CLEAR                   WLBDA8
089100960117     C                   Z-ADD     WHHDAT        G02DAT
089200960117     C                   CALL      'XSRDA8'
089300960117     C                   PARM                    WLBDA8
089400960117     C                   Z-ADD     G02INV        WOGGI             8 0
089500960117     C                   Z-ADD     G02INV        WDTSTA            8 0
089600960117     C                   ELSE
089700960117     C                   Z-ADD     D80DST        WDTSTA
089800960117     C                   END
089900960117      *  Giro data stampa/ristampa
090000960117     C                   Z-ADD     WDTSTA        G02INV
090100960117     C                   MOVEL     '3'           G02ERR
090200960117     C                   CALL      'XSRDA8'
090300960117     C                   PARM                    WLBDA8
090400960118     C                   Z-ADD     G02DAT        STCDTS
090500960117      *----------------------------------------------*
090600960117      *  Eseguo caricamento tab.7R
090700960117     C                   Z-ADD     0             X                 3 0
090800960117     C                   Z-ADD     1             KKUT
090900960117     C                   MOVEL     '7R'          KCOD
091000960117     C     KTAB          CHAIN     TABEL00F                           96
091100960117     C     *IN96         DOWEQ     '0'
091200960117     C     X             ANDLT     100
091300960117     C                   MOVEL     TBLUNI        DS7R
091400960117     C     TBLFLG        IFEQ      ' '
091500960117     C     §7R1BF        ANDNE     *BLANKS
091600960117     C                   ADD       1             X
091700960117     C                   MOVEL     TBLKEY        C7R(X)
091800960117     C                   MOVEL     §7R1BF        T7R(X)
091900960117     C                   END
092000960117     C     KTAB          READE     TABEL00F                               96
092100960117     C                   END
092200960117     C     *IN96         IFEQ      '0'
092300960117     C                   SETON                                        H1
092400960117     C                   GOTO      FINE
092500960117     C                   END
092600960117      *----------------------------------------------*
092700040319      *  Eseguo caricamento tab.7Q
092800040319     c                   clear                   c7Q
092900040319     c                   clear                   d7Q
093000040319     c                   clear                   d7W
093100040319      *
093200040319     C                   Z-ADD     0             X                 3 0
093300040319     C                   Z-ADD     1             KKUT
093400040319     C                   MOVEL     '7Q'          KCOD
093500040319     C     KTAB          CHAIN     TABEL00F                           96
093600040319     C                   Dow       *IN96 = *off and X < 100
093700040319     C                   if        tblFLG = *blank
093800040319     C                   add       1             X
093900040319     C                   MOVEL     TBLKEY        C7Q(X)
094000040319     C                   MOVEL     TBLUNI        D7Q(X)
094100040319     C                   end
094200040319     C     KTAB          READE     TABEL00F                               96
094300040319     C                   End
094400040319      *
094500040319      * carica il proseguimento della tabella
094600040319      *  Eseguo caricamento tab.7W
094700040319     c                   do        100           X
094800040319     c                   if        c7q(X) = *blank
094900040319     c                   leave
095000040319     c                   end
095100040319      *
095200040319      * carica il proseguimento della tabella se c'è
095300040319     C                   Z-ADD     1             KKUT
095400040319     C                   MOVEL     '7W'          KCOD
095500040319     C                   movel(p)  c7q(x)        Kkey
095600040319     C     KTAB1         CHAIN     TABEL00F                           96
095700040319     C                   clear                   D7W(X)
095800040319     C  n96              MOVEL     TBLUNI        D7W(X)
095900040319     c                   endDO
096000120625      /free
096100120625
096200120625         // -?Inschieramento codici bolla con porto franco da tab. "3A"?
096300120626         clear  $3A;
096400120626         clear  x;
096500120625         kkut = 1;
096600120625         kcod = '3A';
096700120625         setll  (kkut : kcod)  TABEL;
096800120625         reade  (kkut : kcod)  TABEL;
096900120625         DoW  Not %eof(TABEL00F);
097000120625           ds3A = TBLuni;
097100120625           if  TBLflg = *blank  and  %subst( §3Atb1 : 1 : 1 ) = 'F';
097200120625             x += 1;
097300120625             $3A(x) = TBLkey;
097400120625           endif;
097500120625           reade  (kkut : kcod)  TABEL;
097600120625         EndDo;
097700120625
097800120625         // -?Aggancio tabella "VPO" per reperimento?
097900120625         //  ?n° pagine limite in stampa LdV (per close/open PrtF)?
098000120625         clear  dVPO;
098100120710         clear  k05tntbe01;
098200120710         k_TBEcod = 'VPO';
098300120710         k_TBEke1 = 'VPO';
098400120710         chain  %kds( k05tntbe01 : 3 )  TNTBE000;
098500120710         if  %found(TNTBE01L);
098600120710           dVPO = TBEuni;
098700120625         else;
098800120625           §VPOpagLdV = 150;
098900120625         endif;
099000120625
099100120625      /end-free
099200040319      *----------------------------------------------*
099300960117      *  Reperisco filiale
099400960117     C                   Z-ADD     0             STCFIL
099500120710     C     DUTlpo        IFEQ      '2'
099600120710     C     DUTlpo        OREQ      *BLANKS
099700960117     C                   SETOFF                                       05
099800120710     C                   MOVE      DUTpou        STCFIL
099900960117     C                   ELSE
100000960117     C                   SETON                                        05
100100960117     C                   MOVEL     SIMFEL        STCFIL
100200960117     C                   END
100300000225      *  Accendo indicatore 06
100400000225     C                   SETON                                        06
100500960117      *  Decodifico filiale
100600960117     C                   Z-ADD     STCFIL        ORGFIL
100700960117     C                   MOVEL     *BLANKS       ORGDES
100800960117     C     ORGFIL        CHAIN     AZORG01L                           30
100900960117     C                   MOVEL     ORGDES        STDFIL
101000960117      *  Reperisco filiale gestione
101100960117     C                   MOVEL     D80FGS        STCFGS
101200960117     C                   Z-ADD     STCFGS        ORGFIL
101300960117     C                   MOVEL     *BLANKS       ORGDES
101400960117     C     ORGFIL        CHAIN     AZORG01L                           30
101500960117     C                   MOVEL     ORGDES        STDFGS
101600040305     C                   MOVEL     ORGDE6        OG146
101700960117      *  Altri dati
101800960117     C                   MOVEL     NOMPGM        STCPGM
101900960117     C                   MOVEL     *ALL'-'       CMP198
102000960117     C                   MOVEL     *ALL'-'       CM1198
102100991130     C                   SETON                                        04
102200000221      *
102300000221      * Se elaborazione DPD gestisco  apertura vista
102400000221      * logica per data e nr. distinta
102500000221     C     D80DPD        IFEQ      'S'
102600000221     C                   OPEN      FNARBX3L
102700000221      * .. altrimenti gestisco apertiura vista logica
102800000221      * x numero spedizione
102900000221     C                   ELSE
103000000221     C                   OPEN      FNARBX1L
103100000221     C                   END
103200960117      *----------------------------------------------*
103300960117     C                   ENDSR
103400960117      *----------------------------------------------*
103500960117     OFNARBX00  E            AGGDAT
103600960117     O                       ARBDST
103700000221     OFNARBX3   E            AGGDA3
103800000221     O                       ARBDST
