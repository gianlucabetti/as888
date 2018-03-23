000100160216     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000200160216     h dftactgrp(*no) actgrp(*caller)
000300160216      *-----------------------------------------------------*
000400160217     FFIBARDPD0FuF   E             DISK
000500160609     FFIBARDPD1FuF a E             DISK    infds(BARds)
000600160216      *---------
000700160624     fdppdc01l  if   e           k disk
000800160217     FEDIvaB2L  UF   E           K DISK
000900160217     FEDIvaT3L  Uf A E           K DISK
001000160216      *---------
001100160608     Ffiar404L  if   E           K DISK    rename(fiar4000:fiar404)
001200160608     Ffnarb01L  if   E           K DISK
001300160831     Ffnlbl01L  if   E           K DISK
001400160608      *---------
001500160217     FEdiVAT4L  if   E           K DISK    rename(EDIVAT00:EDIVAT4)
001600160216     FazORG01L  IF   E           K DISK
001700940128     D*----------------------------------------------------*
001800160609      *
001900160609     d BARds           ds
002000160609     d  BAR_nrr              397    400b 0
002100160609      *
002200160609     D*-------------
002300160217     D KPJBA         E DS
002400160217     D*------------
002500160211     D DVate         E DS
002600160211     D OG143         E DS
002700160217     D*------------
002800160217     D TRUL06DS      E DS
002900160217     D  LIN                    1     90  0 DIM(30)
003000160212     D*------------
003100160212     D* DS REPERIMENTO DATI UTENTE
003200160211     D*-------------
003300160212     d fieuLPDds     e ds
003400160212     d  dLPD         e ds
003500160212     D*-------------
003600160129     d  Sosp_AAS       s                   like(VATAAS)
003700160129     d  Sosp_LNP       s                   like(VATLNP)
003800160129     d  Sosp_NRS       s                   like(VATNRS)
003900160129     d  Sosp_NSP       s                   like(VATNSP)
004000160212     D*-------------
004100160609     d savarbaas       s                   like(arbAAS)
004200160609     d savarblnp       s                   like(arbLNP)
004300160609     d savarbnrs       s                   like(arbNRS)
004400160609     d savarbnsp       s                   like(arbNSP)
004500160609     d savarblna       s                   like(arbLNA)
004600160609     D*-------------
004700160129     D GEO_in_Sospeso  C                   '§DPD_IN_SOSPESO'
004800160201     D*-------------
004900160609     d utente          s             10    INZ(*user)
005000160201     D W0140           S             14  0
005100160201     D WDTGIO          S              8  0
005200160201     D DATEU           S              8  0
005300160201     D DATA_eur        S               D   DATFMT(*eur)
005400160201     D DATA_iso        S               D   DATFMT(*iso)
005500160212     D*-------------
005600160212     D data8alfa       DS
005700160212     d     anno4               1      4
005800160212     d     mese2               5      6
005900160212     d   giorno2               7      8
006000160216     D*-------------
006100160217      *   Parametri ricevuti
006200160216     D*-------------
006300160216     d Chiudi          s              1
006400160216     d Sistema         s             15
006500160624      *
006600160624     D digits          C                   CONST('0123456789')
006700940127     C*----------------------------------------------------*
006800940127     C*                MAIN LINE PROGRAM
006900940127     C*----------------------------------------------------*
007000160216     c     *entry        plist
007100160216     c                   parm                    Chiudi
007200160216     c                   parm                    Sistema
007300160901     c                   parm                    KPJBA
007400160211      *
007500160211      * UDATE IN GGMMAAAA
007600160217     C                   TIME                    W0140
007700160211     C                   MOVE      W0140         WDTGIO
007800160609     C                   MOVEl     W0140         oreminsec         6 0
007900160211     C     *eur          MOVEL     WDTGIO        DATA_eur
008000160211     c                   move      DATA_eur      DATA_iso
008100160211     c                   move      DATA_iso      data_iso8         8 0
008200160608     c                   subdur    1:*m          DATA_iso
008300160608     c                   move      DATA_iso      udata_1mese       8 0
008400160217      /free
008500160217       //--------------------------------------------------------------
008600160217       //?Operazioni iniziali.
008700160217       //--------------------------------------------------------------
008800160217         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
008900160217
009000160217       //?Se chiusura del sistema devo chiudere il lavoro
009100160217         IF  %shtdn;
009200160217           Chiudi = 'S';
009300160217           exsr RoutEnd;
009400160217         ENDIF;
009500160217
009600160217      /end-free
009700160217       //--------------------------------------------------------------
009800160216     c                   do        *hival
009900160216     c                   read(e)   FIBARDPD0F
010000160217      **
010100160216     c                   if        %error
010200160216     c                   iter
010300160216     c                   end
010400160219      **
010500160216     c                   if        %eof(fibardpd0f)
010600160216     c                   leave
010700160216     c                   end
010800160927      **
010900170911      ** Se non è x l'Italia "380" codice ISO nazione o "023" codice BRT B.U.
011000170911      **       salta in fondo   fino
011100170608      **    all'eventuale cancellazione del record se più vecchio di 1 mese
011200170911     c                   if        %subst(bardpdPD1:26:3) = '380' or
011300170911     c                             %subst(bardpdPD1:26:3) = '023'
011400160216      **
011500160216     c                   clear                   sav_linea         3 0
011600160216     c                   clear                   sav_cliente       7 0
011700160217      *--------
011800160217      * CARICO TABELLA PUNTI OPERATIVI GESTITI £1
011900160217     C                   CLEAR                   TRUL06DS
012000160217     C                   MOVE      '£1'          D06COD
012100160217     C                   MOVEL     BARDPDFGS     D06KEY
012200160217     C                   MOVEL     'L'           D06TLA
012300160217     C                   MOVEL     TRUL06DS      KPJBU
012400160217     C                   CALL      'TRUL06R'
012500160217     C                   PARM                    KPJBA
012600160217     c                   MOVEL     KPJBU         TRUL06DS
012700160216      *
012800160216     C* con la Linea di chi spara prende quella DPD da sostituire su LNP e FGS
012900160216     c     BARDPDFGS     chain     azORG01l
013000160216      *
013100160216     c                   if           not %Found(azORG01l) or orgfva<>' '
013200160219      **   Filiali non trovate su organigramma (pulire il record)
013300160219     c                   delete    BARDPD000
013400160216     c                   iter
013500160219      **
013600160216     c                   else
013700160216     c                   movel     orgde3        OG143
013800160216     c                   end
013900160216      *
014000160216     c                   if           §OGLNPB =  *blank or
014100160216     c                                §OGLNPB =  '000'
014200160219      **   Filiali che NON gestiscono DPD      (pulire il record)
014300160219     c                   delete    BARDPD000
014400160216     c                   iter
014500160219      **
014600160216     c                   else
014700160216     c                   move      §OGLNPB       sav_linea
014800160216     c                   end
014900160216      *
015000160216      *   se la LINEA è valida
015100160216     c                   clear                   FIEULPDDS
015200160216     c                   z-add     sav_linea     ILPDLNP
015300160216     c                   z-add     sav_linea     ILPDLNA
015400160216     c*   il FIEULPDR
015500160216     c                   call      'FIEULPDR'
015600160216     c                   parm                    FIEULPDDS
015700160216     c                   eval      dLPD = oLPDUNI
015800160216     c                   move      §LPDksc       sav_cliente       7 0
015900160216     c                   move      ILPDLNP       sav_linea
016000160216      **
016100160211     c                   clear                   abilitato         1
016200160211     C                   movel     bardpdPRN     Parcel           14
016300160524     C                   movel     bardpdPD1     CHISONO_DPD      28
016400160211      *  Controlla e abilita
016500160211     c                   exsr      Check_Parcel
016600160211      *
016700170608     c                   end
016800170608      *
016900160211      * abilitato significa anche abilitato alla cancellazione del record
017000160211      * da spunte oltre al fatto che se la spunta è piu vecchia di 2 mesi
017100160211      * la cancelliamo.
017200160211     c                   if        abilitato = 'S'
017300160211     c                               or
017400160608     c                             udata_1mese > bardpdDFS
017500160211     c                   delete    BARDPD000
017600160211     c                   end
017700160216      *
017800160211     c                   enddo
017900170608      **
018000170608      *=============================
018100170608      **  Esegue la pulizia del FIBARDPD1F x i resi di quelli più vecchi
018200170608      **   di 1 mese
018300170608     c                   do        *hival
018400170608     c                   read(e)   FIBARDPD1F
018500170608      **
018600170608     c                   if        %error
018700170608     c                   iter
018800170608     c                   end
018900170608      **
019000170608     c                   if        %eof(fibardpd1f)
019100170608     c                   leave
019200170608     c                   end
019300170608      **
019400170608      * la cancelliamo.
019500170608     c                   if        udata_1mese > BR1DPDDFS
019600170608     c                   delete    BARDPD001
019700170608     c                   end
019800170608     c                   enddo
019900170608      *--------------
020000160216     c                   exsr      RoutEnd
020100160211      *  ================================================================== */
020200160211      *     CONTROLLA SE il parcel è arrivato su EDIVAB  /FIVAB o BLP
020300160211      *  ================================================================== */
020400160211     c     Check_Parcel  Begsr
020500160212      **
020600160212     c                   clear                   data8alfa
020700160212     c                   clear                   data8             8 0
020800160211      **
020900160211      **  se non ancora confermato CMR  xché in Sospeso
021000160211     C                   eval        vatTRC = 'E'
021100160211     C                   eval        vatNOT = Parcel
021200160211     C     Kvat4E        setll     edivat4l
021300160211     C     vatTRC        reade     edivat4l
021400160211      *
021500160211     c                   IF        not %EoF(edivat4l)
021600160211     c                   movel     vatNOT        dVATe
021700160211      *
021800160211     C                   if        Parcel = §vatEPRN
021900160211     c     vatLNP        chain     azorg01l
022000160211      *
022100160211     c                   if        %Found(azorg01l)
022200160211     c                   eval      OG143 = orgDE3
022300160211      *
022400160211      *  Solo quelle in sospeso vengono aggiornate
022500160212     c                   if        §OGNTW = 'DPD'
022600160212     c                               and
022700160212     c                             (%subst(vatCMR:1:4) = 'GEO_'
022800160212     c                                or
022900160212     c                              vatCMR = GEO_in_SOSPESO)
023000160217      *
023100160217      * se era già stato sparato (quindi è un doppione ...deve solo eliminare il record)
023200160217     c                   if        %subst(vatCMR:15:4) = '_PST'
023300160217     c                   eval       abilitato = 'S'
023400160217     c                   else
023500160212      **
023600160212      * se si tratta di UNA DATA da impostare x il paragone con UDATE
023700160212      ** imposta la data in DATA8
023800160212     c                   if        vatCMR <> GEO_IN_SOSPESO  and
023900160212     c                             %subst(vatCMR:09:1) = '/' and
024000160212     c                             %subst(vatCMR:12:1) = '/'
024100160212     c                   eval          anno4 = %subst(vatCMR:05:4)
024200160212     c                   eval          mese2 = %subst(vatCMR:10:2)
024300160212     c                   eval        giorno2 = %subst(vatCMR:13:2)
024400160212     c                   move      data8alfa     data8
024500160212     c                   end
024600160211      *
024700160211     c                   eval      Sosp_AAS = vatAAS
024800160211     c                   eval      Sosp_LNP = vatLNP
024900160211     c                   eval      Sosp_NRS = vatNRS
025000160211     c                   eval      Sosp_NSP = vatNSP
025100160211      *
025200160211     c                   exsr      ABILITA_CMR
025300160211     c                   leavesr
025400160211      *
025500160217     c                   end
025600160211     c                   end
025700160211     c                   end
025800160211     c                   end
025900160211     c                   end
026000160217      **
026100160217      ** se deve eliminare il record perchè ha già elaborato
026200160217     c                   if          abilitato = 'S'
026300160217     c                   leavesr
026400160217     c                   end
026500160722      **
026600160722      **
026700160722      **
026800160722      **
026900160722      **
027000160211      **
027100160211      **  altrimenti prova a controllare esistenza su FIVAB o BLP
027200160211      /free
027300160211         // 2° test su file CLIENTE
027400160211           EXEC SQL SELECT 'S' INTO :abilitato
027500160211                    FROM FIVAT00F WHERE VATTRC='E' AND
027600160211                         SUBSTR(VATNOT, 1, 14)= :parcel
027700160211                    FETCH FIRST 1 ROW ONLY
027800160211                   ;
027900160211
028000160211                   IF sqlCode < 0;
028100160211                     DUMP(A);
028200160211                   ENDIF;
028300160211                   IF sqlCode = 0 and abilitato = 'S';
028400160211                     Leavesr;
028500160211                   ENDIF;
028600160211
028700160524         // 3° test su file AR4 Bolle GIA' CONFERMATE
028800160211           EXEC SQL SELECT 'S' INTO :abilitato
028900160524                    FROM FIAR400F WHERE AR4TRC='I' AND
029000160524                         SUBSTR(AR4NOT, 1, 14)= :parcel
029100160211                    FETCH FIRST 1 ROW ONLY
029200160211                   ;
029300160211
029400160211                   IF sqlCode < 0;
029500160211                     DUMP(A);
029600160211                   ENDIF;
029700160211                   IF sqlCode = 0 and abilitato = 'S';
029800160608
029900160608                      exsr Check_RESO;
030000160608
030100160211                     Leavesr;
030200160211                   ENDIF;
030300160524
030400160524         // 4° test sui segnacolli BOLLE (x intercettare i clienti DPD fuori circuito)
030500160524         //    che arrivano dall'Olanda direttamente ed importati con un traduttore
030600160524         //    a parte  e NON dal GEODATA - x evitare di scrivere dei VEDI PACCO
030700160524           EXEC SQL SELECT 'S' INTO :abilitato
030800160524                    FROM FIARS00F WHERE ARSTRC='C' AND
030900160524                         SUBSTR(ARSNOT, 1, 28)= :chisono_DPD
031000160524                    FETCH FIRST 1 ROW ONLY
031100160524                   ;
031200160524
031300160524                   IF sqlCode < 0;
031400160524                     DUMP(A);
031500160524                   ENDIF;
031600160524                   IF sqlCode = 0 and abilitato = 'S';
031700160524                     Leavesr;
031800160524                   ENDIF;
031900160524         //
032000160316         //    se non era sui file controllati sopra scrive una bolla
032100160316         //    molto particolare sull'EDIVAB con solo le informazioni
032200160316         //    ricavate dall'etichetta. CAP+PARCEL+SERV.CODE delle solo 380 Italia
032300160316         //
032400160316         // Solo destinatario Italia
032500160316      /end-free
032600160722      **
032700160722      **
032800160722      **
032900160722      **
033000160722      **
033100160722      **
033200170911      **            380 = Italia o codice 023 B.U. BRT di destino
033300170911     c                   if        %subst(bardpdPD1:26:3) = '380' or
033400170911     c                             %subst(bardpdPD1:26:3) = '023'
033500170911      **
033600160927     c                   clear                   kpjbu
033700160316     c                   call      'FIEU21R1'
033800160318     c                   parm                    bardpdFGS
033900160318     c                   parm                    bardpdPD1
034000160316     c                   parm                    abilitato
034100160901     c                   parm                    kpjba
034200170911      **
034300160316     c                   endif
034400160316      **
034500160211     c                   endSR
034600160211      ************************************************************
034700160211      * ABILITA x CMR in SOSPESO immediatamente la SPEDIZIONE alla CONFERMA
034800160211      ************************************************************
034900160129     C     ABILITA_CMR   BEGSR
035000160129      *
035100160129     c                   clear                   new_CMR          35
035200160211     c                   move      'S'           abilitato         1
035300160201      *
035400160211     c                   eval      new_CMR ='GEO_' +
035500160216     c                              %editw(data_iso8:'    /  /  ') + '_PST'
035600160129      *
035700160211      *  per chiave bolla aggiorna la bolla in SOSPENZIONE
035800160129     C     KSosp_EDIVA   setll     EDIVAT3L
035900160129     C     KSosp_EDIVA   reade     EDIVAT3L
036000160129     c                   DOw       not %EoF(EDIVAT3L)
036100160129     c                   eval      vatCMR  =  new_CMR
036200160217      *-
036300160217      *  cerca la LINEA FRA QUELLE presenti
036400160217     c     vatLNP        lookup    LIN                                    25
036500160217     c                   if        not *in25
036600160212     c                   eval      vatLNP =  sav_linea
036700160212     c                   eval      vatFGS =  sav_linea
036800160212     c                   eval      vatCCM =  sav_cliente
036900160217     c                   end
037000160217      *-
037100160129     c                   Update    EDIVAT00
037200160217      *-
037300160129     C     KSosp_EDIVA   reade     EDIVAT3L
037400160129     c                   EndDO
037500160129      *
037600160129      *  poi allinea anche la testata bolla del VAB
037700160129     C     KSosp_EDIVA   chain     EDIVAB2L
037800160129     c                   if        %Found(EDIVAB2L)
037900160210      *** Provenienza  x distinguere i records dall'IGATEAVIS
038000160211     c                   movel(P)  'V'           VABscl
038100160212      * se NON era più in sospensione
038200160212     c                   if        data8 > 0
038300160212     c                   if        data8 > data_iso8
038400160212      * Futuro
038500160212     c                   movel(P)  'F'           VABscl
038600160212     c                   else
038700160212      * Anteriore (può essere anche oggi)
038800160212     c                   movel(P)  'A'           VABscl
038900160212     c                   endif
039000160212     c                   endif
039100160212      **
039200160129     c                   eval      vabCMR  =  new_CMR
039300160217      *
039400160217      *  cerca la LINEA FRA QUELLE presenti
039500160217     c     vabLNP        lookup    LIN                                    25
039600160217     c                   if        not *in25
039700160212     c                   eval      vabLNP =  sav_linea
039800160212     c                   eval      vabFGS =  sav_linea
039900160212     c                   eval      vabCCM =  sav_cliente
040000160217     c                   endif
040100160217      *
040200160201     c                   Update    EDIVAB00
040300160129     c                   End
040400160129      *
040500160129     c                   Endsr
040600160129     C************************************************************
040700160129     C* OPERAZIONI INIZIALI
040800160129     C************************************************************
040900160129     C     *INZSR        BEGSR
041000160211     C*
041100160211     C     Kvat4E        KLIST
041200160211     C                   KFLD                    vatTRC
041300160211     C                   KFLD                    vatNOT
041400160211      *
041500160831     C     Kar4          KLIST
041600160609     C                   KFLD                    ar4AAS
041700160609     C                   KFLD                    ar4LNP
041800160609     C                   KFLD                    ar4NRS
041900160609     C                   KFLD                    ar4NSP
042000160609      *
042100160211     C     KSosp_EDIVA   KLIST
042200160211     C                   KFLD                    Sosp_AAS
042300160211     C                   KFLD                    Sosp_LNP
042400160211     C                   KFLD                    Sosp_NRS
042500160211     C                   KFLD                    Sosp_NSP
042600160608     C*
042700160608     C     Kar4I         KLIST
042800160608     C                   KFLD                    ar4TRC
042900160608     C                   KFLD                    ar4N14
043000160211      *
043100940117     C                   ENDSR
043200160216     C*-------------------------------------------------------------*
043300160216      *    x terminare
043400160216      *  ------------------------------------------------------------------ */
043500160216     C     ROUTEND       BEGSR
043600160216     C
043700160216     C* fine programma
043800160216     C                   SETON                                            LR
043900160216     c                   RETURN
044000160216      **
044100160216     C                   ENDSR
044200160608     C*-------------------------------------------------------------*
044300160608      *    controlla se una bolla precedente era una bolla EXPORT x DPD
044400160608      *  ------------------------------------------------------------------ */
044500160608     C     Check_RESO    BEGSR
044600160608      **
044700160609      ** chiave da salvare  uscendo dal ciclo dell'AR4/ARB
044800160609     c                   clear                   savarbaas
044900160609     c                   clear                   savarblnp
045000160609     c                   clear                   savarbnrs
045100160609     c                   clear                   savarbnsp
045200160609     c                   clear                   savarblna
045300160609     c                   clear                   trovato_ARB       1
045400170120     c                   clear                   imposta_STS       1
045500160609     c                   move      'S'           Consegnate_ALL    1
045600160609      **
045700160608      *  Se è un RESO deve scrivere  il FILE FIBARDPD1F con i riferimenti
045800160608      *     della bolla export  prima di cancellare il record SPIA.
045900160608      **
046000160608      **  deve trovare una sola bolla su ARB non consegnata con LNA = DPD
046100160608      **   se invece ne trova più di una oppure le trova tutte consegnate
046200160608      **  deve scrivere
046300160608     C                   move      'I'           ar4TRC
046400160608     C                   movel     parcel        ar4N14
046500160608     C     Kar4I         setll     fiar404l
046600160608     C     Kar4I         reade     fiar404l
046700160608     c                   dow       not %EoF(fiar404l)
046800160831      **
046900160831      ** cerca l'Originale legata  (perchè potrebbe essere un rimpallo)
047000160831     C     Kar4          chain     fnlbl01l
047100160831      * e se c'è sostituisce la chiave per controllare ARB
047200160831     c                   if        %Found(fnlbl01l)
047300160831     C                   move      LBLAAO        ar4AAS
047400160831     C                   move      LBLLPO        ar4LNP
047500160831     C                   move      LBLNRO        ar4NRS
047600160831     C                   move      LBLNSO        ar4NSP
047700160831     c                   end
047800160831      **
047900160609      ** ARB.......
048000160831     C     Kar4          chain     fnarb01l
048100160609      **
048200160609     c                   if        %Found(fnarb01l)
048300160609     c     arbLNA        chain     azorg01l
048400160609     c                   if        %Found(azorg01l)
048500160609     c                   eval      OG143 = orgDE3
048600160609      ** se la LNA era una DPD
048700160609     c                   if        §OGNTW = 'DPD'
048800160609     c                   eval        trovato_ARB = 'S'
048900160609      *
049000160609     c                   if        arbDCM = 0
049100160609     c                   clear                   Consegnate_ALL
049200160609      *
049300160609     c                   if             savarbaas = 0
049400170120      *
049500160609     c                   eval        savarbaas = arbaas
049600160609     c                   eval        savarblnp = arblnp
049700160609     c                   eval        savarbnrs = arbnrs
049800160609     c                   eval        savarbnsp = arbnsp
049900160609     c                   eval        savarblna = arblna
050000170120      *
050100160609      * altrimenti se ce n'è più di una da consegnare occorre lasciare
050200160609      * vuota la chiave bolla.
050300160609     c                   else
050400170120      *
050500170120      **---  20/1/2017    lascio comunque la chiave bolla  aggiungo lo STATUS 9 come segnale
050600170120     c                   move      '9'           imposta_STS       1
050700170120     c****               clear                   savarbaas
050800170120     c****               clear                   savarblnp
050900170120     c****               clear                   savarbnrs
051000170120     c****               clear                   savarbnsp
051100170120      **---  20/1/2017    lascio comunque la chiave bolla
051200160609     c                   leave
051300170120      *
051400160609     c                   end
051500160609      *
051600160615     c                   else
051700170120      *
051800170120      *  SE BOLLA GIA' CONSEGNATA
051900160615     c                   eval        savarblna = arblna
052000170120      **---  20/1/2017
052100170120      **  da adesso reimposto la chiave bolla. fino ad oggi NON si voleva avere su
052200170120      **  questi records il riferimento della nostra spedizione bensì solo la LNA
052300170120     c                   move      '9'           imposta_STS       1
052400170120      **  aggiungo lo STATUS 9 come segnale
052500170120     c                   eval        savarbaas = arbaas
052600170120     c                   eval        savarblnp = arblnp
052700170120     c                   eval        savarbnrs = arbnrs
052800170120     c                   eval        savarbnsp = arbnsp
052900170120     c                   eval        savarblna = arblna
053000170120      **---
053100160609     c                   end
053200160608      **
053300160609     c                   end
053400160609     c                   end
053500160609     c                   end
053600160609      **
053700160608     C     Kar4I         reade     fiar404l
053800160608     c                   end
053900160609      **
054000160609      ** usciti dal ciclo ma con tutte le sped consegnate deve
054100160609      **  svuotare i campi salvati
054200160613     c                   IF          trovato_ARB    = 'S'
054300160613      **
054400160613     c                   IF          Consegnate_ALL = 'S'
054500170120      **
054600170120      **---  20/1/2017    lascio comunque la chiave bolla  aggiungo lo STATUS 9 come segnale
054700170120     c                   move      '9'           imposta_STS       1
054800170120     c****               clear                   savarbaas
054900170120     c****               clear                   savarblnp
055000170120     c****               clear                   savarbnrs
055100170120     c****               clear                   savarbnsp
055200170120      **---  20/1/2017    lascio comunque la chiave bolla
055300170120      **
055400160609     c                   end
055500160608      **
055600160609      **  Se si è trovato un ARB con LNA DPD significa che abbiamo un RESO
055700160609      **  fra i barcode segnacolli, quindi deve scrivere un record sul
055800160609      **  file dei resi  FIBARDPD1F
055900160609     c                   clear                   BAR_nrr
056000160609      *
056100160609      *  Controlla presenza già del parcel
056200160609      /free
056300160609           EXEC SQL SELECT rrn(FIBARDPD1F) INTO :BAR_nrr
056400160609                    FROM FIBARDPD1F WHERE  BR1DPDPRN = :Parcel
056500160609                    FETCH FIRST 1 ROW ONLY
056600160609                   ;
056700160609      /end-free
056800160609      *
056900160609      ** se lo ha trovato è in UPDATE altrimenti è in write
057000160609     c                   if        sqlCOD = 0 and BAR_nrr > 0
057100160609     c     BAR_nrr       chain     fiBARDPD1F
057200160609     c                   if        %Found(fiBARDPD1F)
057300160609     c                   eval      BR1DPDAAS   = savarbaas
057400160609     c                   eval      BR1DPDLNP   = savarblnp
057500160609     c                   eval      BR1DPDNRS   = savarbnrs
057600160609     c                   eval      BR1DPDNSP   = savarbnsp
057700160609     c                   eval      BR1DPDLNA   = savarblna
057800160609     c                   update    BARDPD001
057900160609     c                   end
058000160609     c                   else
058100160610     c                   clear                   BARDPD001
058200160609     c                   eval      BR1DPDPD1   =   BARDPDPD1
058300160609     c                   eval      BR1DPDPRN   =   BARDPDPRN
058400160609     c                   eval      BR1DPDFGS   =   BARDPDFGS
058500160609     c                   eval      BR1DPDDFS   =   BARDPDDFS
058600160609     c                   eval      BR1DPDHMS   =   BARDPDHMS
058700160609     c                   eval      BR1DPDAAS   = savarbaas
058800160609     c                   eval      BR1DPDLNP   = savarblnp
058900160609     c                   eval      BR1DPDNRS   = savarbnrs
059000160609     c                   eval      BR1DPDNSP   = savarbnsp
059100160609     c                   eval      BR1DPDLNA   = savarblna
059200160624      **
059300160624      **** scrivo solo se il Parcel ha un DEPOT valido per DPD
059400160822     c                   movel     BR1DPDPRN     wparcel          14
059500160624     c                   exsr      chkdepot
059600160624      **
059700160624     c                   if        scrivere = 'S'
059800160609     c                   write     BARDPD001
059900160624     c                   endif
060000160624      **
060100160609     c                   end
060200160609      **
060300160613     c                   endIF
060400160613      **
060500160608     C                   ENDSR
060600160624      *______________________________________________________________
060700160624     c     chkdepot      begsr
060800160624      *______________________________________________________________
060900160624     c                   move      'N'           scrivere          1
061000160624     c                   setoff                                       35
061100160624     c     digits        check     wparcel                                35
061200160624     C                   if        not *in35
061300160624     c                   movel     wparcel       k4                4
061400160624     c     k4            setll     dppdc01l
061500160624     c     k4            reade     dppdc01l
061600160624     c                   dow       not %EoF(dppdc01l)
061700160624     c                   if            pdcdsc >= data_iso8
061800160624     c                   move      'S'           scrivere
061900160624     C                   leave
062000160624     C                   endif
062100160624     c     k4            reade     dppdc01l
062200160624     C                   endDO
062300160624     C                   endif
062400160624     c                   endsr
062500160216      *  ------------------------------------------------------------------ */
062600160216      *       X non bloccare in nessun caso il traduttore
062700160216      *  ------------------------------------------------------------------ */
062800160216     C     *pssr         BEGSR
062900160216     C
063000160216      **
063100160216     C                   ENDSR     '*CANCL'
063200160216      * ?------------------------------------------------------------------ */
