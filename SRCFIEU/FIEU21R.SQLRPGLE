000100160216     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000200160216     h dftactgrp(*no) actgrp(*caller)
000300160216      *-----------------------------------------------------*
000400160217     FFIBARDPD0FuF   E             DISK
000500160609     FFIBARDPD1FuF a E             DISK    infds(BARds)
000600160216      *---------
000700160217     FEDIvaB2L  UF   E           K DISK
000800160217     FEDIvaT3L  Uf A E           K DISK
000900160216      *---------
001000171011     Ffipnd03l  if   E           K DISK
001100160608     Ffnarb01L  if   E           K DISK
001200160831     Ffnlbl01L  if   E           K DISK
001300160608      *---------
001400160217     FEdiVAT4L  if   E           K DISK    rename(EDIVAT00:EDIVAT4)
001500160216     FazORG01L  IF   E           K DISK
001600940128     D*----------------------------------------------------*
001700160609      *
001800160609     d BARds           ds
001900160609     d  BAR_nrr              397    400b 0
002000160609      *
002100160609     D*-------------
002200160217     D KPJBA         E DS
002300160217     D*------------
002400160211     D DVate         E DS
002500160211     D OG143         E DS
002600160217     D*------------
002700171010     D TruldepDS     E DS
002800171010     D*------------
002900160217     D TRUL06DS      E DS
003000160217     D  LIN                    1     90  0 DIM(30)
003100160212     D*------------
003200160212     D* DS REPERIMENTO DATI UTENTE
003300160211     D*-------------
003400160212     d fieuLPDds     e ds
003500160212     d  dLPD         e ds
003600160212     D*-------------
003700160129     d  Sosp_AAS       s                   like(VATAAS)
003800160129     d  Sosp_LNP       s                   like(VATLNP)
003900160129     d  Sosp_NRS       s                   like(VATNRS)
004000160129     d  Sosp_NSP       s                   like(VATNSP)
004100160212     D*-------------
004200160609     d savarbaas       s                   like(arbAAS)
004300160609     d savarblnp       s                   like(arbLNP)
004400160609     d savarbnrs       s                   like(arbNRS)
004500160609     d savarbnsp       s                   like(arbNSP)
004600160609     d savarblna       s                   like(arbLNA)
004700160609     D*-------------
004800160129     D GEO_in_Sospeso  C                   '§DPD_IN_SOSPESO'
004900160201     D*-------------
005000160609     d utente          s             10    INZ(*user)
005100160201     D W0140           S             14  0
005200160201     D WDTGIO          S              8  0
005300160201     D DATEU           S              8  0
005400160201     D DATA_eur        S               D   DATFMT(*eur)
005500160201     D DATA_iso        S               D   DATFMT(*iso)
005600160212     D*-------------
005700160212     D data8alfa       DS
005800160212     d     anno4               1      4
005900160212     d     mese2               5      6
006000160212     d   giorno2               7      8
006100160216     D*-------------
006200160217      *   Parametri ricevuti
006300160216     D*-------------
006400160216     d Chiudi          s              1
006500160216     d Sistema         s             15
006600160624      *
006700160624     D digits          C                   CONST('0123456789')
006800940127     C*----------------------------------------------------*
006900940127     C*                MAIN LINE PROGRAM
007000940127     C*----------------------------------------------------*
007100160216     c     *entry        plist
007200160216     c                   parm                    Chiudi
007300160216     c                   parm                    Sistema
007400160901     c                   parm                    KPJBA
007500160211      *
007600160211      * UDATE IN GGMMAAAA
007700160217     C                   TIME                    W0140
007800160211     C                   MOVE      W0140         WDTGIO
007900160609     C                   MOVEl     W0140         oreminsec         6 0
008000160211     C     *eur          MOVEL     WDTGIO        DATA_eur
008100160211     c                   move      DATA_eur      DATA_iso
008200160211     c                   move      DATA_iso      data_iso8         8 0
008300160608     c                   subdur    1:*m          DATA_iso
008400160608     c                   move      DATA_iso      udata_1mese       8 0
008500160217      /free
008600160217       //--------------------------------------------------------------
008700160217       //?Operazioni iniziali.
008800160217       //--------------------------------------------------------------
008900160217         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
009000160217
009100160217       //?Se chiusura del sistema devo chiudere il lavoro
009200160217         IF  %shtdn;
009300160217           Chiudi = 'S';
009400160217           exsr RoutEnd;
009500160217         ENDIF;
009600160217
009700160217      /end-free
009800160217       //--------------------------------------------------------------
009900160216     c                   do        *hival
010000160216     c                   read(e)   FIBARDPD0F
010100160217      **
010200160216     c                   if        %error
010300160216     c                   iter
010400160216     c                   end
010500160219      **
010600160216     c                   if        %eof(fibardpd0f)
010700160216     c                   leave
010800160216     c                   end
010900160927      **
011000170911      ** Se non è x l'Italia "380" codice ISO nazione o "023" codice BRT B.U.
011100170911      **       salta in fondo   fino
011200170608      **    all'eventuale cancellazione del record se più vecchio di 1 mese
011300170911     c                   if        %subst(bardpdPD1:26:3) = '380' or
011400170911     c                             %subst(bardpdPD1:26:3) = '023'
011500160216      **
011600160216     c                   clear                   sav_linea         3 0
011700160216     c                   clear                   sav_cliente       7 0
011800160217      *--------
011900160217      * CARICO TABELLA PUNTI OPERATIVI GESTITI £1
012000160217     C                   CLEAR                   TRUL06DS
012100160217     C                   MOVE      '£1'          D06COD
012200160217     C                   MOVEL     BARDPDFGS     D06KEY
012300160217     C                   MOVEL     'L'           D06TLA
012400160217     C                   MOVEL     TRUL06DS      KPJBU
012500160217     C                   CALL      'TRUL06R'
012600160217     C                   PARM                    KPJBA
012700160217     c                   MOVEL     KPJBU         TRUL06DS
012800160216      *
012900160216     C* con la Linea di chi spara prende quella DPD da sostituire su LNP e FGS
013000160216     c     BARDPDFGS     chain     azORG01l
013100160216      *
013200160216     c                   if           not %Found(azORG01l) or orgfva<>' '
013300160219      **   Filiali non trovate su organigramma (pulire il record)
013400160219     c                   delete    BARDPD000
013500160216     c                   iter
013600160219      **
013700160216     c                   else
013800160216     c                   movel     orgde3        OG143
013900160216     c                   end
014000160216      *
014100160216     c                   if           §OGLNPB =  *blank or
014200160216     c                                §OGLNPB =  '000'
014300160219      **   Filiali che NON gestiscono DPD      (pulire il record)
014400160219     c                   delete    BARDPD000
014500160216     c                   iter
014600160219      **
014700160216     c                   else
014800160216     c                   move      §OGLNPB       sav_linea
014900160216     c                   end
015000160216      *
015100160216      *   se la LINEA è valida
015200160216     c                   clear                   FIEULPDDS
015300160216     c                   z-add     sav_linea     ILPDLNP
015400160216     c                   z-add     sav_linea     ILPDLNA
015500160216     c*   il FIEULPDR
015600160216     c                   call      'FIEULPDR'
015700160216     c                   parm                    FIEULPDDS
015800160216     c                   eval      dLPD = oLPDUNI
015900160216     c                   move      §LPDksc       sav_cliente       7 0
016000160216     c                   move      ILPDLNP       sav_linea
016100160216      **
016200160211     c                   clear                   abilitato         1
016300160211     C                   movel     bardpdPRN     Parcel           14
016400160524     C                   movel     bardpdPD1     CHISONO_DPD      28
016500160211      *  Controlla e abilita
016600160211     c                   exsr      Check_Parcel
016700160211      *
016800170608     c                   end
016900170608      *
017000160211      * abilitato significa anche abilitato alla cancellazione del record
017100160211      * da spunte oltre al fatto che se la spunta è piu vecchia di 2 mesi
017200160211      * la cancelliamo.
017300160211     c                   if        abilitato = 'S'
017400160211     c                               or
017500160608     c                             udata_1mese > bardpdDFS
017600160211     c                   delete    BARDPD000
017700160211     c                   end
017800160216      *
017900160211     c                   enddo
018000170608      **
018100170608      *=============================
018200170608      **  Esegue la pulizia del FIBARDPD1F x i resi di quelli più vecchi
018300170608      **   di 1 mese
018400170608     c                   do        *hival
018500170608     c                   read(e)   FIBARDPD1F
018600170608      **
018700170608     c                   if        %error
018800170608     c                   iter
018900170608     c                   end
019000170608      **
019100170608     c                   if        %eof(fibardpd1f)
019200170608     c                   leave
019300170608     c                   end
019400170608      **
019500170608      * la cancelliamo.
019600170608     c                   if        udata_1mese > BR1DPDDFS
019700170608     c                   delete    BARDPD001
019800170608     c                   end
019900170608     c                   enddo
020000170608      *--------------
020100160216     c                   exsr      RoutEnd
020200160211      *  ================================================================== */
020300160211      *     CONTROLLA SE il parcel è arrivato su EDIVAB  /FIVAB o BLP
020400160211      *  ================================================================== */
020500160211     c     Check_Parcel  Begsr
020600160212      **
020700160212     c                   clear                   data8alfa
020800160212     c                   clear                   data8             8 0
020900160211      **
021000160211      **  se non ancora confermato CMR  xché in Sospeso
021100160211     C                   eval        vatTRC = 'E'
021200160211     C                   eval        vatNOT = Parcel
021300160211     C     Kvat4E        setll     edivat4l
021400160211     C     vatTRC        reade     edivat4l
021500160211      *
021600160211     c                   IF        not %EoF(edivat4l)
021700160211     c                   movel     vatNOT        dVATe
021800160211      *
021900160211     C                   if        Parcel = §vatEPRN
022000160211     c     vatLNP        chain     azorg01l
022100160211      *
022200160211     c                   if        %Found(azorg01l)
022300160211     c                   eval      OG143 = orgDE3
022400160211      *
022500160211      *  Solo quelle in sospeso vengono aggiornate
022600160212     c                   if        §OGNTW = 'DPD'
022700160212     c                               and
022800160212     c                             (%subst(vatCMR:1:4) = 'GEO_'
022900160212     c                                or
023000160212     c                              vatCMR = GEO_in_SOSPESO)
023100160217      *
023200160217      * se era già stato sparato (quindi è un doppione ...deve solo eliminare il record)
023300160217     c                   if        %subst(vatCMR:15:4) = '_PST'
023400160217     c                   eval       abilitato = 'S'
023500160217     c                   else
023600160212      **
023700160212      * se si tratta di UNA DATA da impostare x il paragone con UDATE
023800160212      ** imposta la data in DATA8
023900160212     c                   if        vatCMR <> GEO_IN_SOSPESO  and
024000160212     c                             %subst(vatCMR:09:1) = '/' and
024100160212     c                             %subst(vatCMR:12:1) = '/'
024200160212     c                   eval          anno4 = %subst(vatCMR:05:4)
024300160212     c                   eval          mese2 = %subst(vatCMR:10:2)
024400160212     c                   eval        giorno2 = %subst(vatCMR:13:2)
024500160212     c                   move      data8alfa     data8
024600160212     c                   end
024700160211      *
024800160211     c                   eval      Sosp_AAS = vatAAS
024900160211     c                   eval      Sosp_LNP = vatLNP
025000160211     c                   eval      Sosp_NRS = vatNRS
025100160211     c                   eval      Sosp_NSP = vatNSP
025200160211      *
025300160211     c                   exsr      ABILITA_CMR
025400160211     c                   leavesr
025500160211      *
025600160217     c                   end
025700160211     c                   end
025800160211     c                   end
025900160211     c                   end
026000160211     c                   end
026100160217      **
026200160217      ** se deve eliminare il record perchè ha già elaborato
026300160217     c                   if          abilitato = 'S'
026400160217     c                   leavesr
026500160217     c                   end
026600160722      **
026700160722      **
026800160722      **
026900160722      **
027000160722      **
027100160211      **
027200160211      **  altrimenti prova a controllare esistenza su FIVAB o BLP
027300160211      /free
027400160211         // 2° test su file CLIENTE
027500160211           EXEC SQL SELECT 'S' INTO :abilitato
027600160211                    FROM FIVAT00F WHERE VATTRC='E' AND
027700160211                         SUBSTR(VATNOT, 1, 14)= :parcel
027800160211                    FETCH FIRST 1 ROW ONLY
027900160211                   ;
028000160211
028100160211                   IF sqlCode < 0;
028200160211                     DUMP(A);
028300160211                   ENDIF;
028400160211                   IF sqlCode = 0 and abilitato = 'S';
028500160211                     Leavesr;
028600160211                   ENDIF;
028700160211
028800171011         // 3° test su file PND ex ar4(I) Bolle GIA' CONFERMATE
028900160211           EXEC SQL SELECT 'S' INTO :abilitato
029000171011                    FROM FIPND00F WHERE PNDDGS = 0 AND
029100171011                         PNDIPN = :parcel
029200160211                    FETCH FIRST 1 ROW ONLY
029300160211                   ;
029400160211
029500160211                   IF sqlCode < 0;
029600160211                     DUMP(A);
029700160211                   ENDIF;
029800160211                   IF sqlCode = 0 and abilitato = 'S';
029900160608
030000160608                      exsr Check_RESO;
030100160608
030200160211                     Leavesr;
030300160211                   ENDIF;
030400160524
030500160524         // 4° test sui segnacolli BOLLE (x intercettare i clienti DPD fuori circuito)
030600160524         //    che arrivano dall'Olanda direttamente ed importati con un traduttore
030700160524         //    a parte  e NON dal GEODATA - x evitare di scrivere dei VEDI PACCO
030800160524           EXEC SQL SELECT 'S' INTO :abilitato
030900160524                    FROM FIARS00F WHERE ARSTRC='C' AND
031000160524                         SUBSTR(ARSNOT, 1, 28)= :chisono_DPD
031100160524                    FETCH FIRST 1 ROW ONLY
031200160524                   ;
031300160524
031400160524                   IF sqlCode < 0;
031500160524                     DUMP(A);
031600160524                   ENDIF;
031700160524                   IF sqlCode = 0 and abilitato = 'S';
031800160524                     Leavesr;
031900160524                   ENDIF;
032000160524         //
032100160316         //    se non era sui file controllati sopra scrive una bolla
032200160316         //    molto particolare sull'EDIVAB con solo le informazioni
032300160316         //    ricavate dall'etichetta. CAP+PARCEL+SERV.CODE delle solo 380 Italia
032400160316         //
032500160316         // Solo destinatario Italia
032600160316      /end-free
032700160722      **
032800160722      **
032900160722      **
033000160722      **
033100160722      **
033200160722      **
033300170911      **            380 = Italia o codice 023 B.U. BRT di destino
033400170911     c                   if        %subst(bardpdPD1:26:3) = '380' or
033500170911     c                             %subst(bardpdPD1:26:3) = '023'
033600170911      **
033700160927     c                   clear                   kpjbu
033800160316     c                   call      'FIEU21R1'
033900160318     c                   parm                    bardpdFGS
034000160318     c                   parm                    bardpdPD1
034100160316     c                   parm                    abilitato
034200160901     c                   parm                    kpjba
034300170911      **
034400160316     c                   endif
034500160316      **
034600160211     c                   endSR
034700160211      ************************************************************
034800160211      * ABILITA x CMR in SOSPESO immediatamente la SPEDIZIONE alla CONFERMA
034900160211      ************************************************************
035000160129     C     ABILITA_CMR   BEGSR
035100160129      *
035200160129     c                   clear                   new_CMR          35
035300160211     c                   move      'S'           abilitato         1
035400160201      *
035500160211     c                   eval      new_CMR ='GEO_' +
035600160216     c                              %editw(data_iso8:'    /  /  ') + '_PST'
035700160129      *
035800160211      *  per chiave bolla aggiorna la bolla in SOSPENZIONE
035900160129     C     KSosp_EDIVA   setll     EDIVAT3L
036000160129     C     KSosp_EDIVA   reade     EDIVAT3L
036100160129     c                   DOw       not %EoF(EDIVAT3L)
036200160129     c                   eval      vatCMR  =  new_CMR
036300160217      *-
036400160217      *  cerca la LINEA FRA QUELLE presenti
036500160217     c     vatLNP        lookup    LIN                                    25
036600160217     c                   if        not *in25
036700160212     c                   eval      vatLNP =  sav_linea
036800160212     c                   eval      vatFGS =  sav_linea
036900160212     c                   eval      vatCCM =  sav_cliente
037000160217     c                   end
037100160217      *-
037200160129     c                   Update    EDIVAT00
037300160217      *-
037400160129     C     KSosp_EDIVA   reade     EDIVAT3L
037500160129     c                   EndDO
037600160129      *
037700160129      *  poi allinea anche la testata bolla del VAB
037800160129     C     KSosp_EDIVA   chain     EDIVAB2L
037900160129     c                   if        %Found(EDIVAB2L)
038000160210      *** Provenienza  x distinguere i records dall'IGATEAVIS
038100160211     c                   movel(P)  'V'           VABscl
038200160212      * se NON era più in sospensione
038300160212     c                   if        data8 > 0
038400160212     c                   if        data8 > data_iso8
038500160212      * Futuro
038600160212     c                   movel(P)  'F'           VABscl
038700160212     c                   else
038800160212      * Anteriore (può essere anche oggi)
038900160212     c                   movel(P)  'A'           VABscl
039000160212     c                   endif
039100160212     c                   endif
039200160212      **
039300160129     c                   eval      vabCMR  =  new_CMR
039400160217      *
039500160217      *  cerca la LINEA FRA QUELLE presenti
039600160217     c     vabLNP        lookup    LIN                                    25
039700160217     c                   if        not *in25
039800160212     c                   eval      vabLNP =  sav_linea
039900160212     c                   eval      vabFGS =  sav_linea
040000160212     c                   eval      vabCCM =  sav_cliente
040100160217     c                   endif
040200160217      *
040300160201     c                   Update    EDIVAB00
040400160129     c                   End
040500160129      *
040600160129     c                   Endsr
040700160129     C************************************************************
040800160129     C* OPERAZIONI INIZIALI
040900160129     C************************************************************
041000160129     C     *INZSR        BEGSR
041100160211     C*
041200160211     C     Kvat4E        KLIST
041300160211     C                   KFLD                    vatTRC
041400160211     C                   KFLD                    vatNOT
041500160211      *
041600171011     C     Kpnd          KLIST
041700171011     C                   KFLD                    pndAAS
041800171011     C                   KFLD                    pndLNP
041900171011     C                   KFLD                    pndNRS
042000171011     C                   KFLD                    pndNSP
042100160609      *
042200160211     C     KSosp_EDIVA   KLIST
042300160211     C                   KFLD                    Sosp_AAS
042400160211     C                   KFLD                    Sosp_LNP
042500160211     C                   KFLD                    Sosp_NRS
042600160211     C                   KFLD                    Sosp_NSP
042700160211      *
042800940117     C                   ENDSR
042900160216     C*-------------------------------------------------------------*
043000160216      *    x terminare
043100160216      *  ------------------------------------------------------------------ */
043200160216     C     ROUTEND       BEGSR
043300160216     C
043400160216     C* fine programma
043500160216     C                   SETON                                            LR
043600160216     c                   RETURN
043700160216      **
043800160216     C                   ENDSR
043900160608     C*-------------------------------------------------------------*
044000160608      *    controlla se una bolla precedente era una bolla EXPORT x DPD
044100160608      *  ------------------------------------------------------------------ */
044200160608     C     Check_RESO    BEGSR
044300160608      **
044400171011      ** chiave da salvare  uscendo dal ciclo dell'PND/ARB
044500160609     c                   clear                   savarbaas
044600160609     c                   clear                   savarblnp
044700160609     c                   clear                   savarbnrs
044800160609     c                   clear                   savarbnsp
044900160609     c                   clear                   savarblna
045000160609     c                   clear                   trovato_ARB       1
045100170120     c                   clear                   imposta_STS       1
045200160609     c                   move      'S'           Consegnate_ALL    1
045300160609      **
045400160608      *  Se è un RESO deve scrivere  il FILE FIBARDPD1F con i riferimenti
045500160608      *     della bolla export  prima di cancellare il record SPIA.
045600160608      **
045700160608      **  deve trovare una sola bolla su ARB non consegnata con LNA = DPD
045800160608      **   se invece ne trova più di una oppure le trova tutte consegnate
045900160608      **  deve scrivere
046000171011     C     parcel        setll     fipnd03l
046100171011     C     parcel        reade     fipnd03l
046200171011     c                   dow       not %EoF(fipnd03l)
046300160831      **
046400160831      ** cerca l'Originale legata  (perchè potrebbe essere un rimpallo)
046500171011     C     Kpnd          chain     fnlbl01l
046600160831      * e se c'è sostituisce la chiave per controllare ARB
046700160831     c                   if        %Found(fnlbl01l)
046800171011     C                   move      LBLAAO        pndAAS
046900171011     C                   move      LBLLPO        pndLNP
047000171011     C                   move      LBLNRO        pndNRS
047100171011     C                   move      LBLNSO        pndNSP
047200160831     c                   end
047300160831      **
047400160609      ** ARB.......
047500171011     C     Kpnd          chain     fnarb01l
047600160609      **
047700160609     c                   if        %Found(fnarb01l)
047800160609     c     arbLNA        chain     azorg01l
047900160609     c                   if        %Found(azorg01l)
048000160609     c                   eval      OG143 = orgDE3
048100160609      ** se la LNA era una DPD
048200160609     c                   if        §OGNTW = 'DPD'
048300160609     c                   eval        trovato_ARB = 'S'
048400160609      *
048500160609     c                   if        arbDCM = 0
048600160609     c                   clear                   Consegnate_ALL
048700160609      *
048800160609     c                   if             savarbaas = 0
048900170120      *
049000160609     c                   eval        savarbaas = arbaas
049100160609     c                   eval        savarblnp = arblnp
049200160609     c                   eval        savarbnrs = arbnrs
049300160609     c                   eval        savarbnsp = arbnsp
049400160609     c                   eval        savarblna = arblna
049500170120      *
049600160609      * altrimenti se ce n'è più di una da consegnare occorre lasciare
049700160609      * vuota la chiave bolla.
049800160609     c                   else
049900170120      *
050000170120      **---  20/1/2017    lascio comunque la chiave bolla  aggiungo lo STATUS 9 come segnale
050100170120     c                   move      '9'           imposta_STS       1
050200170120     c****               clear                   savarbaas
050300170120     c****               clear                   savarblnp
050400170120     c****               clear                   savarbnrs
050500170120     c****               clear                   savarbnsp
050600170120      **---  20/1/2017    lascio comunque la chiave bolla
050700160609     c                   leave
050800170120      *
050900160609     c                   end
051000160609      *
051100160615     c                   else
051200170120      *
051300170120      *  SE BOLLA GIA' CONSEGNATA
051400160615     c                   eval        savarblna = arblna
051500170120      **---  20/1/2017
051600170120      **  da adesso reimposto la chiave bolla. fino ad oggi NON si voleva avere su
051700170120      **  questi records il riferimento della nostra spedizione bensì solo la LNA
051800170120     c                   move      '9'           imposta_STS       1
051900170120      **  aggiungo lo STATUS 9 come segnale
052000170120     c                   eval        savarbaas = arbaas
052100170120     c                   eval        savarblnp = arblnp
052200170120     c                   eval        savarbnrs = arbnrs
052300170120     c                   eval        savarbnsp = arbnsp
052400170120     c                   eval        savarblna = arblna
052500170120      **---
052600160609     c                   end
052700160608      **
052800160609     c                   end
052900160609     c                   end
053000160609     c                   end
053100160609      **
053200171011     C     parcel        reade     fipnd03l
053300160608     c                   end
053400160609      **
053500160609      ** usciti dal ciclo ma con tutte le sped consegnate deve
053600160609      **  svuotare i campi salvati
053700160613     c                   IF          trovato_ARB    = 'S'
053800160613      **
053900160613     c                   IF          Consegnate_ALL = 'S'
054000170120      **
054100170120      **---  20/1/2017    lascio comunque la chiave bolla  aggiungo lo STATUS 9 come segnale
054200170120     c                   move      '9'           imposta_STS       1
054300170120     c****               clear                   savarbaas
054400170120     c****               clear                   savarblnp
054500170120     c****               clear                   savarbnrs
054600170120     c****               clear                   savarbnsp
054700170120      **---  20/1/2017    lascio comunque la chiave bolla
054800170120      **
054900160609     c                   end
055000160608      **
055100160609      **  Se si è trovato un ARB con LNA DPD significa che abbiamo un RESO
055200160609      **  fra i barcode segnacolli, quindi deve scrivere un record sul
055300160609      **  file dei resi  FIBARDPD1F
055400160609     c                   clear                   BAR_nrr
055500160609      *
055600160609      *  Controlla presenza già del parcel
055700160609      /free
055800160609           EXEC SQL SELECT rrn(FIBARDPD1F) INTO :BAR_nrr
055900160609                    FROM FIBARDPD1F WHERE  BR1DPDPRN = :Parcel
056000160609                    FETCH FIRST 1 ROW ONLY
056100160609                   ;
056200160609      /end-free
056300160609      *
056400160609      ** se lo ha trovato è in UPDATE altrimenti è in write
056500160609     c                   if        sqlCOD = 0 and BAR_nrr > 0
056600160609     c     BAR_nrr       chain     fiBARDPD1F
056700160609     c                   if        %Found(fiBARDPD1F)
056800160609     c                   eval      BR1DPDAAS   = savarbaas
056900160609     c                   eval      BR1DPDLNP   = savarblnp
057000160609     c                   eval      BR1DPDNRS   = savarbnrs
057100160609     c                   eval      BR1DPDNSP   = savarbnsp
057200160609     c                   eval      BR1DPDLNA   = savarblna
057300160609     c                   update    BARDPD001
057400160609     c                   end
057500160609     c                   else
057600160610     c                   clear                   BARDPD001
057700160609     c                   eval      BR1DPDPD1   =   BARDPDPD1
057800160609     c                   eval      BR1DPDPRN   =   BARDPDPRN
057900160609     c                   eval      BR1DPDFGS   =   BARDPDFGS
058000160609     c                   eval      BR1DPDDFS   =   BARDPDDFS
058100160609     c                   eval      BR1DPDHMS   =   BARDPDHMS
058200160609     c                   eval      BR1DPDAAS   = savarbaas
058300160609     c                   eval      BR1DPDLNP   = savarblnp
058400160609     c                   eval      BR1DPDNRS   = savarbnrs
058500160609     c                   eval      BR1DPDNSP   = savarbnsp
058600160609     c                   eval      BR1DPDLNA   = savarblna
058700160624      **
058800160624      **** scrivo solo se il Parcel ha un DEPOT valido per DPD
058900160822     c                   movel     BR1DPDPRN     wparcel          14
059000160624     c                   exsr      chkdepot
059100160624      **
059200160624     c                   if        scrivere = 'S'
059300160609     c                   write     BARDPD001
059400160624     c                   endif
059500160624      **
059600160609     c                   end
059700160609      **
059800160613     c                   endIF
059900160613      **
060000160608     C                   ENDSR
060100160624      *______________________________________________________________
060200160624     c     chkdepot      begsr
060300160624      *______________________________________________________________
060400160624     c                   move      'N'           scrivere          1
060500160624     c                   setoff                                       35
060600160624     c     digits        check     wparcel                                35
060700171010      ***
060800160624     C                   if        not *in35
060900171010      ***
061000171010      ***  Controlla esistenza sui LEGAMI con depot a 4
061100171010     C                   clear                   TRULDEPds
061200171010     c                   move      'L'           IdepTLA
061300171010     c                   moveL     wparcel       IdepDPC                        1mi 4 caratteri
061400171010     c                   z-add     data_iso8     IdepDRF
061500171010     c                   call      'TRULDEPR'
061600171010     C                   PARM                    TRULDEPds
061700171010      *
061800171010     C                   IF        OdepERR <>'1'
061900171010     c                   move      'S'           scrivere
062000171010     C                   endif
062100171010      ***
062200160624     C                   endif
062300171010      ***
062400160624     c                   endsr
062500160216      *  ------------------------------------------------------------------ */
062600160216      *       X non bloccare in nessun caso il traduttore
062700160216      *  ------------------------------------------------------------------ */
062800160216     C     *pssr         BEGSR
062900160216     C
063000160216      **
063100160216     C                   ENDSR     '*CANCL'
063200160216      * ?------------------------------------------------------------------ */
