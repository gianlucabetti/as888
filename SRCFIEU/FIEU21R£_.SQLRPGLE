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
010900170608      ** Se non è x l'Italia salta in fondo
010901170608      **       fino
010902170608      **    all'eventuale cancellazione del record se più vecchio di 1 mese
011000170608     c                   if        %subst(bardpdPD1:26:3) = '380'
011300160216      **
011400160216     c                   clear                   sav_linea         3 0
011500160216     c                   clear                   sav_cliente       7 0
011600160217      *--------
011700160217      * CARICO TABELLA PUNTI OPERATIVI GESTITI £1
011800160217     C                   CLEAR                   TRUL06DS
011900160217     C                   MOVE      '£1'          D06COD
012000160217     C                   MOVEL     BARDPDFGS     D06KEY
012100160217     C                   MOVEL     'L'           D06TLA
012200160217     C                   MOVEL     TRUL06DS      KPJBU
012300160217     C                   CALL      'TRUL06R'
012400160217     C                   PARM                    KPJBA
012500160217     c                   MOVEL     KPJBU         TRUL06DS
012600160216      *
012700160216     C* con la Linea di chi spara prende quella DPD da sostituire su LNP e FGS
012800160216     c     BARDPDFGS     chain     azORG01l
012900160216      *
013000160216     c                   if           not %Found(azORG01l) or orgfva<>' '
013100160219      **   Filiali non trovate su organigramma (pulire il record)
013200160219     c                   delete    BARDPD000
013300160216     c                   iter
013400160219      **
013500160216     c                   else
013600160216     c                   movel     orgde3        OG143
013700160216     c                   end
013800160216      *
013900160216     c                   if           §OGLNPB =  *blank or
014000160216     c                                §OGLNPB =  '000'
014100160219      **   Filiali che NON gestiscono DPD      (pulire il record)
014200160219     c                   delete    BARDPD000
014300160216     c                   iter
014400160219      **
014500160216     c                   else
014600160216     c                   move      §OGLNPB       sav_linea
014700160216     c                   end
014800160216      *
014900160216      *   se la LINEA è valida
015000160216     c                   clear                   FIEULPDDS
015100160216     c                   z-add     sav_linea     ILPDLNP
015200160216     c                   z-add     sav_linea     ILPDLNA
015300160216     c*   il FIEULPDR
015400160216     c                   call      'FIEULPDR'
015500160216     c                   parm                    FIEULPDDS
015600160216     c                   eval      dLPD = oLPDUNI
015700160216     c                   move      §LPDksc       sav_cliente       7 0
015800160216     c                   move      ILPDLNP       sav_linea
015900160216      **
016000160211     c                   clear                   abilitato         1
016100160211     C                   movel     bardpdPRN     Parcel           14
016200160524     C                   movel     bardpdPD1     CHISONO_DPD      28
016300160211      *  Controlla e abilita
016400160211     c                   exsr      Check_Parcel
016500160211      *
016501170608     c                   end
016502170608      *
016600160211      * abilitato significa anche abilitato alla cancellazione del record
016700160211      * da spunte oltre al fatto che se la spunta è piu vecchia di 2 mesi
016800160211      * la cancelliamo.
016900160211     c                   if        abilitato = 'S'
017000160211     c                               or
017100160608     c                             udata_1mese > bardpdDFS
017200160211     c                   delete    BARDPD000
017300160211     c                   end
017400160216      *
017500160211     c                   enddo
017501170608      **
017600170608      *=============================
017601170608      **  Esegue la pulizia del FIBARDPD1F x i resi di quelli più vecchi
017602170608      **   di 1 mese
017603170608     c                   do        *hival
017604170608     c                   read(e)   FIBARDPD1F
017605170608      **
017606170608     c                   if        %error
017607170608     c                   iter
017608170608     c                   end
017609170608      **
017610170608     c                   if        %eof(fibardpd1f)
017611170608     c                   leave
017612170608     c                   end
017613170608      **
017614170608      * la cancelliamo.
017615170608     c                   if        udata_1mese > BR1DPDDFS
017618170608     c                   delete    BARDPD001
017619170608     c                   end
017620170608     c                   enddo
017621170608      *--------------
017700160216     c                   exsr      RoutEnd
017800160211      *  ================================================================== */
017900160211      *     CONTROLLA SE il parcel è arrivato su EDIVAB  /FIVAB o BLP
018000160211      *  ================================================================== */
018100160211     c     Check_Parcel  Begsr
018200160212      **
018300160212     c                   clear                   data8alfa
018400160212     c                   clear                   data8             8 0
018500160211      **
018600160211      **  se non ancora confermato CMR  xché in Sospeso
018700160211     C                   eval        vatTRC = 'E'
018800160211     C                   eval        vatNOT = Parcel
018900160211     C     Kvat4E        setll     edivat4l
019000160211     C     vatTRC        reade     edivat4l
019100160211      *
019200160211     c                   IF        not %EoF(edivat4l)
019300160211     c                   movel     vatNOT        dVATe
019400160211      *
019500160211     C                   if        Parcel = §vatEPRN
019600160211     c     vatLNP        chain     azorg01l
019700160211      *
019800160211     c                   if        %Found(azorg01l)
019900160211     c                   eval      OG143 = orgDE3
020000160211      *
020100160211      *  Solo quelle in sospeso vengono aggiornate
020200160212     c                   if        §OGNTW = 'DPD'
020300160212     c                               and
020400160212     c                             (%subst(vatCMR:1:4) = 'GEO_'
020500160212     c                                or
020600160212     c                              vatCMR = GEO_in_SOSPESO)
020700160217      *
020800160217      * se era già stato sparato (quindi è un doppione ...deve solo eliminare il record)
020900160217     c                   if        %subst(vatCMR:15:4) = '_PST'
021000160217     c                   eval       abilitato = 'S'
021100160217     c                   else
021200160212      **
021300160212      * se si tratta di UNA DATA da impostare x il paragone con UDATE
021400160212      ** imposta la data in DATA8
021500160212     c                   if        vatCMR <> GEO_IN_SOSPESO  and
021600160212     c                             %subst(vatCMR:09:1) = '/' and
021700160212     c                             %subst(vatCMR:12:1) = '/'
021800160212     c                   eval          anno4 = %subst(vatCMR:05:4)
021900160212     c                   eval          mese2 = %subst(vatCMR:10:2)
022000160212     c                   eval        giorno2 = %subst(vatCMR:13:2)
022100160212     c                   move      data8alfa     data8
022200160212     c                   end
022300160211      *
022400160211     c                   eval      Sosp_AAS = vatAAS
022500160211     c                   eval      Sosp_LNP = vatLNP
022600160211     c                   eval      Sosp_NRS = vatNRS
022700160211     c                   eval      Sosp_NSP = vatNSP
022800160211      *
022900160211     c                   exsr      ABILITA_CMR
023000160211     c                   leavesr
023100160211      *
023200160217     c                   end
023300160211     c                   end
023400160211     c                   end
023500160211     c                   end
023600160211     c                   end
023700160217      **
023800160217      ** se deve eliminare il record perchè ha già elaborato
023900160217     c                   if          abilitato = 'S'
024000160217     c                   leavesr
024100160217     c                   end
024200160722      **
024300160722      **
024400160722      **
024500160722      **
024600160722      **
024700160211      **
024800160211      **  altrimenti prova a controllare esistenza su FIVAB o BLP
024900160211      /free
025000160211         // 2° test su file CLIENTE
025100160211           EXEC SQL SELECT 'S' INTO :abilitato
025200160211                    FROM FIVAT00F WHERE VATTRC='E' AND
025300160211                         SUBSTR(VATNOT, 1, 14)= :parcel
025400160211                    FETCH FIRST 1 ROW ONLY
025500160211                   ;
025600160211
025700160211                   IF sqlCode < 0;
025800160211                     DUMP(A);
025900160211                   ENDIF;
026000160211                   IF sqlCode = 0 and abilitato = 'S';
026100160211                     Leavesr;
026200160211                   ENDIF;
026300160211
026400160524         // 3° test su file AR4 Bolle GIA' CONFERMATE
026500160211           EXEC SQL SELECT 'S' INTO :abilitato
026600160524                    FROM FIAR400F WHERE AR4TRC='I' AND
026700160524                         SUBSTR(AR4NOT, 1, 14)= :parcel
026800160211                    FETCH FIRST 1 ROW ONLY
026900160211                   ;
027000160211
027100160211                   IF sqlCode < 0;
027200160211                     DUMP(A);
027300160211                   ENDIF;
027400160211                   IF sqlCode = 0 and abilitato = 'S';
027500160608
027600160608                      exsr Check_RESO;
027700160608
027800160211                     Leavesr;
027900160211                   ENDIF;
028000160524
028100160524         // 4° test sui segnacolli BOLLE (x intercettare i clienti DPD fuori circuito)
028200160524         //    che arrivano dall'Olanda direttamente ed importati con un traduttore
028300160524         //    a parte  e NON dal GEODATA - x evitare di scrivere dei VEDI PACCO
028400160524           EXEC SQL SELECT 'S' INTO :abilitato
028500160524                    FROM FIARS00F WHERE ARSTRC='C' AND
028600160524                         SUBSTR(ARSNOT, 1, 28)= :chisono_DPD
028700160524                    FETCH FIRST 1 ROW ONLY
028800160524                   ;
028900160524
029000160524                   IF sqlCode < 0;
029100160524                     DUMP(A);
029200160524                   ENDIF;
029300160524                   IF sqlCode = 0 and abilitato = 'S';
029400160524                     Leavesr;
029500160524                   ENDIF;
029600160524         //
029700160316         //    se non era sui file controllati sopra scrive una bolla
029800160316         //    molto particolare sull'EDIVAB con solo le informazioni
029900160316         //    ricavate dall'etichetta. CAP+PARCEL+SERV.CODE delle solo 380 Italia
030000160316         //
030100160316         // Solo destinatario Italia
030200160316      /end-free
030300160722      **
030400160722      **
030500160722      **
030600160722      **
030700160722      **
030800160722      **
030900160316      **            380 = Italia
031000160316     c                   if        %subst(bardpdPD1:26:3) = '380'
031100160927     c                   clear                   kpjbu
031200160316     c                   call      'FIEU21R1'
031300160318     c                   parm                    bardpdFGS
031400160318     c                   parm                    bardpdPD1
031500160316     c                   parm                    abilitato
031600160901     c                   parm                    kpjba
031700160316     c                   endif
031800160316      **
031900160211     c                   endSR
032000160211      ************************************************************
032100160211      * ABILITA x CMR in SOSPESO immediatamente la SPEDIZIONE alla CONFERMA
032200160211      ************************************************************
032300160129     C     ABILITA_CMR   BEGSR
032400160129      *
032500160129     c                   clear                   new_CMR          35
032600160211     c                   move      'S'           abilitato         1
032700160201      *
032800160211     c                   eval      new_CMR ='GEO_' +
032900160216     c                              %editw(data_iso8:'    /  /  ') + '_PST'
033000160129      *
033100160211      *  per chiave bolla aggiorna la bolla in SOSPENZIONE
033200160129     C     KSosp_EDIVA   setll     EDIVAT3L
033300160129     C     KSosp_EDIVA   reade     EDIVAT3L
033400160129     c                   DOw       not %EoF(EDIVAT3L)
033500160129     c                   eval      vatCMR  =  new_CMR
033600160217      *-
033700160217      *  cerca la LINEA FRA QUELLE presenti
033800160217     c     vatLNP        lookup    LIN                                    25
033900160217     c                   if        not *in25
034000160212     c                   eval      vatLNP =  sav_linea
034100160212     c                   eval      vatFGS =  sav_linea
034200160212     c                   eval      vatCCM =  sav_cliente
034300160217     c                   end
034400160217      *-
034500160129     c                   Update    EDIVAT00
034600160217      *-
034700160129     C     KSosp_EDIVA   reade     EDIVAT3L
034800160129     c                   EndDO
034900160129      *
035000160129      *  poi allinea anche la testata bolla del VAB
035100160129     C     KSosp_EDIVA   chain     EDIVAB2L
035200160129     c                   if        %Found(EDIVAB2L)
035300160210      *** Provenienza  x distinguere i records dall'IGATEAVIS
035400160211     c                   movel(P)  'V'           VABscl
035500160212      * se NON era più in sospensione
035600160212     c                   if        data8 > 0
035700160212     c                   if        data8 > data_iso8
035800160212      * Futuro
035900160212     c                   movel(P)  'F'           VABscl
036000160212     c                   else
036100160212      * Anteriore (può essere anche oggi)
036200160212     c                   movel(P)  'A'           VABscl
036300160212     c                   endif
036400160212     c                   endif
036500160212      **
036600160129     c                   eval      vabCMR  =  new_CMR
036700160217      *
036800160217      *  cerca la LINEA FRA QUELLE presenti
036900160217     c     vabLNP        lookup    LIN                                    25
037000160217     c                   if        not *in25
037100160212     c                   eval      vabLNP =  sav_linea
037200160212     c                   eval      vabFGS =  sav_linea
037300160212     c                   eval      vabCCM =  sav_cliente
037400160217     c                   endif
037500160217      *
037600160201     c                   Update    EDIVAB00
037700160129     c                   End
037800160129      *
037900160129     c                   Endsr
038000160129     C************************************************************
038100160129     C* OPERAZIONI INIZIALI
038200160129     C************************************************************
038300160129     C     *INZSR        BEGSR
038400160211     C*
038500160211     C     Kvat4E        KLIST
038600160211     C                   KFLD                    vatTRC
038700160211     C                   KFLD                    vatNOT
038800160211      *
038900160831     C     Kar4          KLIST
039000160609     C                   KFLD                    ar4AAS
039100160609     C                   KFLD                    ar4LNP
039200160609     C                   KFLD                    ar4NRS
039300160609     C                   KFLD                    ar4NSP
039400160609      *
039500160211     C     KSosp_EDIVA   KLIST
039600160211     C                   KFLD                    Sosp_AAS
039700160211     C                   KFLD                    Sosp_LNP
039800160211     C                   KFLD                    Sosp_NRS
039900160211     C                   KFLD                    Sosp_NSP
040000160608     C*
040100160608     C     Kar4I         KLIST
040200160608     C                   KFLD                    ar4TRC
040300160608     C                   KFLD                    ar4N14
040400160211      *
040500940117     C                   ENDSR
040600160216     C*-------------------------------------------------------------*
040700160216      *    x terminare
040800160216      *  ------------------------------------------------------------------ */
040900160216     C     ROUTEND       BEGSR
041000160216     C
041100160216     C* fine programma
041200160216     C                   SETON                                            LR
041300160216     c                   RETURN
041400160216      **
041500160216     C                   ENDSR
041600160608     C*-------------------------------------------------------------*
041700160608      *    controlla se una bolla precedente era una bolla EXPORT x DPD
041800160608      *  ------------------------------------------------------------------ */
041900160608     C     Check_RESO    BEGSR
042000160608      **
042100160609      ** chiave da salvare  uscendo dal ciclo dell'AR4/ARB
042200160609     c                   clear                   savarbaas
042300160609     c                   clear                   savarblnp
042400160609     c                   clear                   savarbnrs
042500160609     c                   clear                   savarbnsp
042600160609     c                   clear                   savarblna
042700160609     c                   clear                   trovato_ARB       1
042800170120     c                   clear                   imposta_STS       1
042900160609     c                   move      'S'           Consegnate_ALL    1
043000160609      **
043100160608      *  Se è un RESO deve scrivere  il FILE FIBARDPD1F con i riferimenti
043200160608      *     della bolla export  prima di cancellare il record SPIA.
043300160608      **
043400160608      **  deve trovare una sola bolla su ARB non consegnata con LNA = DPD
043500160608      **   se invece ne trova più di una oppure le trova tutte consegnate
043600160608      **  deve scrivere
043700160608     C                   move      'I'           ar4TRC
043800160608     C                   movel     parcel        ar4N14
043900160608     C     Kar4I         setll     fiar404l
044000160608     C     Kar4I         reade     fiar404l
044100160608     c                   dow       not %EoF(fiar404l)
044200160831      **
044300160831      ** cerca l'Originale legata  (perchè potrebbe essere un rimpallo)
044400160831     C     Kar4          chain     fnlbl01l
044500160831      * e se c'è sostituisce la chiave per controllare ARB
044600160831     c                   if        %Found(fnlbl01l)
044700160831     C                   move      LBLAAO        ar4AAS
044800160831     C                   move      LBLLPO        ar4LNP
044900160831     C                   move      LBLNRO        ar4NRS
045000160831     C                   move      LBLNSO        ar4NSP
045100160831     c                   end
045200160831      **
045300160609      ** ARB.......
045400160831     C     Kar4          chain     fnarb01l
045500160609      **
045600160609     c                   if        %Found(fnarb01l)
045700160609     c     arbLNA        chain     azorg01l
045800160609     c                   if        %Found(azorg01l)
045900160609     c                   eval      OG143 = orgDE3
046000160609      ** se la LNA era una DPD
046100160609     c                   if        §OGNTW = 'DPD'
046200160609     c                   eval        trovato_ARB = 'S'
046300160609      *
046400160609     c                   if        arbDCM = 0
046500160609     c                   clear                   Consegnate_ALL
046600160609      *
046700160609     c                   if             savarbaas = 0
046800170120      *
046900160609     c                   eval        savarbaas = arbaas
047000160609     c                   eval        savarblnp = arblnp
047100160609     c                   eval        savarbnrs = arbnrs
047200160609     c                   eval        savarbnsp = arbnsp
047300160609     c                   eval        savarblna = arblna
047400170120      *
047500160609      * altrimenti se ce n'è più di una da consegnare occorre lasciare
047600160609      * vuota la chiave bolla.
047700160609     c                   else
047800170120      *
047900170120      **---  20/1/2017    lascio comunque la chiave bolla  aggiungo lo STATUS 9 come segnale
048000170120     c                   move      '9'           imposta_STS       1
048100170120     c****               clear                   savarbaas
048200170120     c****               clear                   savarblnp
048300170120     c****               clear                   savarbnrs
048400170120     c****               clear                   savarbnsp
048500170120      **---  20/1/2017    lascio comunque la chiave bolla
048600160609     c                   leave
048700170120      *
048800160609     c                   end
048900160609      *
049000160615     c                   else
049100170120      *
049200170120      *  SE BOLLA GIA' CONSEGNATA
049300160615     c                   eval        savarblna = arblna
049400170120      **---  20/1/2017
049500170120      **  da adesso reimposto la chiave bolla. fino ad oggi NON si voleva avere su
049600170120      **  questi records il riferimento della nostra spedizione bensì solo la LNA
049700170120     c                   move      '9'           imposta_STS       1
049800170120      **  aggiungo lo STATUS 9 come segnale
049900170120     c                   eval        savarbaas = arbaas
050000170120     c                   eval        savarblnp = arblnp
050100170120     c                   eval        savarbnrs = arbnrs
050200170120     c                   eval        savarbnsp = arbnsp
050300170120     c                   eval        savarblna = arblna
050400170120      **---
050500160609     c                   end
050600160608      **
050700160609     c                   end
050800160609     c                   end
050900160609     c                   end
051000160609      **
051100160608     C     Kar4I         reade     fiar404l
051200160608     c                   end
051300160609      **
051400160609      ** usciti dal ciclo ma con tutte le sped consegnate deve
051500160609      **  svuotare i campi salvati
051600160613     c                   IF          trovato_ARB    = 'S'
051700160613      **
051800160613     c                   IF          Consegnate_ALL = 'S'
051900170120      **
052000170120      **---  20/1/2017    lascio comunque la chiave bolla  aggiungo lo STATUS 9 come segnale
052100170120     c                   move      '9'           imposta_STS       1
052200170120     c****               clear                   savarbaas
052300170120     c****               clear                   savarblnp
052400170120     c****               clear                   savarbnrs
052500170120     c****               clear                   savarbnsp
052600170120      **---  20/1/2017    lascio comunque la chiave bolla
052700170120      **
052800160609     c                   end
052900160608      **
053000160609      **  Se si è trovato un ARB con LNA DPD significa che abbiamo un RESO
053100160609      **  fra i barcode segnacolli, quindi deve scrivere un record sul
053200160609      **  file dei resi  FIBARDPD1F
053300160609     c                   clear                   BAR_nrr
053400160609      *
053500160609      *  Controlla presenza già del parcel
053600160609      /free
053700160609           EXEC SQL SELECT rrn(FIBARDPD1F) INTO :BAR_nrr
053800160609                    FROM FIBARDPD1F WHERE  BR1DPDPRN = :Parcel
053900160609                    FETCH FIRST 1 ROW ONLY
054000160609                   ;
054100160609      /end-free
054200160609      *
054300160609      ** se lo ha trovato è in UPDATE altrimenti è in write
054400160609     c                   if        sqlCOD = 0 and BAR_nrr > 0
054500160609     c     BAR_nrr       chain     fiBARDPD1F
054600160609     c                   if        %Found(fiBARDPD1F)
054700160609     c                   eval      BR1DPDAAS   = savarbaas
054800160609     c                   eval      BR1DPDLNP   = savarblnp
054900160609     c                   eval      BR1DPDNRS   = savarbnrs
055000160609     c                   eval      BR1DPDNSP   = savarbnsp
055100160609     c                   eval      BR1DPDLNA   = savarblna
055200160609     c                   update    BARDPD001
055300160609     c                   end
055400160609     c                   else
055500160610     c                   clear                   BARDPD001
055600160609     c                   eval      BR1DPDPD1   =   BARDPDPD1
055700160609     c                   eval      BR1DPDPRN   =   BARDPDPRN
055800160609     c                   eval      BR1DPDFGS   =   BARDPDFGS
055900160609     c                   eval      BR1DPDDFS   =   BARDPDDFS
056000160609     c                   eval      BR1DPDHMS   =   BARDPDHMS
056100160609     c                   eval      BR1DPDAAS   = savarbaas
056200160609     c                   eval      BR1DPDLNP   = savarblnp
056300160609     c                   eval      BR1DPDNRS   = savarbnrs
056400160609     c                   eval      BR1DPDNSP   = savarbnsp
056500160609     c                   eval      BR1DPDLNA   = savarblna
056600160624      **
056700160624      **** scrivo solo se il Parcel ha un DEPOT valido per DPD
056800160822     c                   movel     BR1DPDPRN     wparcel          14
056900160624     c                   exsr      chkdepot
057000160624      **
057100160624     c                   if        scrivere = 'S'
057200160609     c                   write     BARDPD001
057300160624     c                   endif
057400160624      **
057500160609     c                   end
057600160609      **
057700160613     c                   endIF
057800160613      **
057900160608     C                   ENDSR
058000160624      *______________________________________________________________
058100160624     c     chkdepot      begsr
058200160624      *______________________________________________________________
058300160624     c                   move      'N'           scrivere          1
058400160624     c                   setoff                                       35
058500160624     c     digits        check     wparcel                                35
058600160624     C                   if        not *in35
058700160624     c                   movel     wparcel       k4                4
058800160624     c     k4            setll     dppdc01l
058900160624     c     k4            reade     dppdc01l
059000160624     c                   dow       not %EoF(dppdc01l)
059100160624     c                   if            pdcdsc >= data_iso8
059200160624     c                   move      'S'           scrivere
059300160624     C                   leave
059400160624     C                   endif
059500160624     c     k4            reade     dppdc01l
059600160624     C                   endDO
059700160624     C                   endif
059800160624     c                   endsr
059900160216      *  ------------------------------------------------------------------ */
060000160216      *       X non bloccare in nessun caso il traduttore
060100160216      *  ------------------------------------------------------------------ */
060200160216     C     *pssr         BEGSR
060300160216     C
060400160216      **
060500160216     C                   ENDSR     '*CANCL'
060600160216      * ?------------------------------------------------------------------ */
