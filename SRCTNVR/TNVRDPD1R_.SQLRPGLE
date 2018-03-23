000100080116     H**** DFTACTGRP(*NO) BNDDIR('QC2LE')
000200080111     H DECEDIT('0,') DATEDIT(*DMY.)
000300080111
000400060621     FTITA430C  IF   E           K DISK
000500080111     FTITAS30C  IF   E           K DISK    prefix(MAM_)
000600080111     FFNLBL01L  if   e           k disk
000700080111
000800080116     FTNVRDPD1F uf a E           k DISK    rename(DPDTOTALI:tnvrDPD1)
000900080116     FTNVRDPD2F o  a E             DISK    rename(DPDRESI:tnvrDPD2)
001000080111
001100060621     D DS_TAS        E DS                  EXTNAME(TITAS00F) INZ
001200080116     D DSBL4I        E DS                  INZ
001300080111
001400080111     d RPYESITO        s             10S 0                                      Esito.
001500080111     d RPYOPCODE       s             10A                                        Cod Oper.risposta
001600080111     d RPYSTATUS       s             10S 0                                      Stato risposta.
001700080116
001800080116     d TITA4_Trovato   s              1
001900080116     d TiTAS_Trovato   s              1
002000080116     d FnLBL_Trovato   s              1
002100080116
002200080116     d Anno_di4        s              4s 0
002300080116     d Mese_di2        s              2s 0
002400080116
002500080116     d Bolla_Reso      s              1
002600080116     d Filiale_Depot   s              1
002700080121     d Bolla_di        s              3                                         Bolla tipo
002800080116     d ParcelNr        s             14
002900080117     d Parcel_di       s              1
003000080121     d Lunghezza_Parcel...
003100080121     d                 s              2s 0
003200080116
003300080115     d Depot_DPD       s              4
003400080116     d Filiale_Bartolini...
003500080116     d                 s              4
003600080111
003700080111     d acceso          c                   '1'
003800080111     d spento          c                   '0'
003900080111     D ESITO_OK        C                   0
004000080111     D ESITO_WARNING   C                   1
004100080111     D ESITO_ERROR     C                   -1
004200080117     d Import          c                   'Imp'
004300080117     d Export          c                   'Exp'
004400080115     d Bartolini       c                   'B'
004500080115     d DPD             c                   'D'
004600080115     d Bolla           c                   'B'
004700080115     d Reso            c                   'R'
004800080116     d Depot           c                   'D'
004900080116     d Filiale         c                   'F'
005000080111
005100080111      //********************************************************************************************
005200080111      // Definizione chiavi database.
005300080111      //********************************************************************************************
005400080111     D kdsLegami     E DS
005500080111     D                                     EXTNAME(fnLBL01L:*KEY)
005600080111     D                                     INZ
005700080114     D kdsDepot      E DS
005800080111     D                                     EXTNAME(tiTA430C:*KEY)
005900080111     D                                     INZ
006000080111     d   Ta4trc      E
006100080111     d                                     INZ('I')
006200080111     D kdsMamma      E DS
006300080111     D                                     EXTNAME(tiTAS30C:*KEY)
006400080111     D                                     INZ
006500080116     d                                     prefix(MAM_)
006600080114     D kdsTotali     E DS
006700080114     D                                     EXTNAME(TNVRDPD1F:*KEY)
006800080114     D                                     INZ
006900080111
007000080111      *------------------------------------------------------------------------*
007100080111      * Creazione RecordSET bolle DPD  Import/Export
007200080111      *   interessate alla verifica nei 6 mesi precedenti
007300080111      *------------------------------------------------------------------------*
007400080111     C/EXEC SQL
007500080111     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
007600080111     C/END-EXEC
007700080116      *------------------------------------------------------------------------*
007800080111
007900080111      *  In ordine legge prima Bolle Import e poi Export solo DPD
008000080111
008100080111     C/EXEC SQL
008200080111     C+ DECLARE C1 CURSOR FOR
008300080111     C+ SELECT * FROM TITAS10F
008400080111     C+    WHERE TASLNP between 190 and 197 or
008500080111     C+          TASLNA between 190 and 197
008600080111     C+ ORDER BY TASLNP desc
008700080111     C/END-EXEC
008800080111
008900080111     C/EXEC SQL
009000080111     C+ OPEN C1
009100080111     C/END-EXEC
009200080111      /FREE
009300080111
009400080111         IF sqlCode < 0;
009500080111           rpyEsito = ESITO_ERROR;
009600080111           rpyOpCode = 'SQLCODE';
009700080111           rpyStatus = sqlCode;
009800080111           EXSR uscita;
009900080111         ENDIF;
010000080111
010100080111         DOU sqlCod = 100;
010200080111
010300080111      /END-FREE
010400080111     C/EXEC SQL
010500080111     C+ FETCH NEXT FROM C1 INTO :DS_TAS
010600080111     C/END-EXEC
010700080111      /FREE
010800080111
010900080111           SELECT;
011000080111             WHEN sqlCode = 100;
011100080111               LEAVE;
011200080111
011300080111             WHEN sqlCode < 0;
011400080111               rpyEsito = ESITO_ERROR;
011500080111               rpyOpCode = 'SQLCODE';
011600080111               rpyStatus = sqlCode;
011700080111               LEAVE;
011800080111
011900080111             OTHER;
012000080111
012100080111               EXSR run_Row;
012200080116               EXSR Conta_Parcels;
012300080111
012400080111           ENDSL;
012500080111
012600080111         ENDDO;
012700080111
012800080111      /END-FREE
012900080111     C/EXEC SQL
013000080111     C+ CLOSE C1
013100080111     C/END-EXEC
013200080111      /FREE
013300080111
013400080111       EXSR uscita;
013500080111
013600080111       //*******************************************************************************************
013700080111       // Uscita.
013800080111       //*******************************************************************************************
013900080111       BEGSR uscita;
014000080111
014100080111         IF rpyOpCode = *BLANK;
014200080111           rpyOpCode = 'DONE';
014300080111           rpyStatus = ESITO_OK;
014400080111         ENDIF;
014500080111
014600080111         *INLR = *ON;
014700080111         RETURN;
014800080111
014900080111       ENDSR;
015000080111
015100080111       //*******************************************************************************************
015200080111       // Dettaglio
015300080111       //*******************************************************************************************
015400080111       BEGSR Run_Row;
015500080114
015600080116?      //  Anno e Mese
015700080117         Anno_di4 = tasAAS;
015800080116         Mese_di2 = tasMGS/100;
015900080116
016000080117?      //  Import/Export in base alla linea
016100080117         IF  190 <= tasLNP and tasLNP <= 197;             // Linea Partenza DPD -> Import
016200080117           Bolla_di = Import;
016300080116           Filiale_Bartolini  = ' '+ %editc(tasLNA:'X');
016400080117         elSE;                                            // Linea Arrivo   DPD -> Export
016500080117           Bolla_di = Export;
016600080116           Filiale_Bartolini  = ' '+ %editc(tasLNP:'X');
016700080115         endIF;
016800080115
016900080115?      //  trova il Parcel e il Depot   ?
017000080116         CHAIN (tasAAS:tasLNP:tasNRS:tasNSP:'I') tiTA430c;
017100080115
017200080115?        // trovato il nome del parcel e il depot .  ?
017300080115         if %Found(Tita430c);
017400080115             DsBL4i = ta4not;
017500080116             Tita4_trovato = 'S';
017600080116             ParcelNr = §B4IPN;
017700080121             Lunghezza_Parcel = 14;
017800080121             if   %subst(§B4IPN:12:1) = ' ';
017900080121             Lunghezza_Parcel = 11;
018000080121             endIF;
018100080115
018200080115?        // con il nome Parcel si sa di chi è la spedizione
018300080121             if   %subst(§B4IPN:1:4) = '0844' and Lunghezza_Parcel = 14 or
018400080121                  %subst(§B4IPN:1:4) = '0845' and Lunghezza_Parcel = 14 or
018500080121                  %subst(§B4IPN:1:4) = '844'  and Lunghezza_Parcel = 11 or
018600080121                  %subst(§B4IPN:1:4) = '845'  and Lunghezza_Parcel = 11;
018700080117                Parcel_di = Bartolini;
018800080117                Depot_DPD = §B4IDA;                       // Depot di Arrivo   DPD
018900080115             else;
019000080117                Parcel_di = DPD;
019100080121                If Lunghezza_Parcel = 14;
019200080121                   Depot_DPD = %subst(§B4IPN:1:4);           // Depot di Partenza DPD
019300080121                else;
019400080121                   Depot_DPD = '0' + %subst(§B4IPN:1:3);     // Depot di Partenza DPD
019500080121                endIF;
019600080115             endIF;
019700080115
019800080115         elSE;
019900080116             Tita4_trovato = 'N';
020000080117             Parcel_di = *blank;
020100080117             Depot_DPD = *blank;
020200080117             ParcelNr = *blank;
020300080115         endIF;
020400080115
020500080116         //?  controlla se è un Reso o è una Semplice Bolla
020600080115         Bolla_Reso = Bolla;
020700080117         iF  Bolla_di = Import and Parcel_di = Bartolini  or
020800080117             Bolla_di = Export and Parcel_di = DPD;
020900080117             Bolla_Reso = Reso;
021000080117             exsr Controlla_Bolla_Di_Reso;
021100080116         endIF;
021200080115
021300080116
021400080115       ENDSR;
021500080115       //*******************************************************************************************
021600080116       // scrive il file di totali Parcels movimentati sul Depot o sulla filiale
021700080115       //*******************************************************************************************
021800080116       BEGSR Conta_Parcels;
021900080116
022000080117        //?  Conta i parcel x Depot
022100080116       clear totParcels;
022200080116       clear totResi;
022300080116
022400080117       CHAIN (Bolla_di:Depot_DPD:Anno_di4:Mese_di2) tnvrDPD1f;
022500080117
022600080116       if Bolla_Reso = Reso;
022700080116             totResi = totResi + 1;
022800080117       else;
022900080117             totParcels = totParcels + 1;
023000080117       endIF;
023100080116
023200080116       if %Found(tnvrDPD1f);
023300080116         update tnvrDPD1;
023400080116       else;
023500080117         Imp_o_Exp = Bolla_di;
023600080117         NomeDepot = Depot_DPD;
023700080116         Anno      = Anno_di4;
023800080116         Mese      = Mese_di2;
023900080116         write  tnvrDPD1;
024000080116       endif;
024100080116
024200080116       ENDSR;
024300080116       //*******************************************************************************************
024400080116       // x Bolla di Reso verifica i dati della Mamma
024500080116       //*******************************************************************************************
024600080116       BEGSR Controlla_Bolla_Di_Reso;
024700080116
024800080116         clear tnvrDPD2;
024900080116         tiTAS_Trovato = 'N';
025000080116         fnLBL_Trovato = 'N';
025100080117
025200080117         // inverte il senso
025300080117         If Bolla_di = Import;
025400080117            Imp_Exp  = Export;
025500080117         else;
025600080117            Imp_Exp  = Import;
025700080117         endif;
025800080117
025900080117         CodDepot = Depot_DPD;
026000080116         Anno_Mese= %char(Anno_di4) + '/' + %editc(Mese_di2:'X');
026100080116         Parcel   = ParcelNr;
026200080116         reso_Ann = tasAAS;
026300080116         reso_LNP = tasLNP;
026400080116         reso_NRS = tasNRS;
026500080116         reso_NSP = tasNSP;
026600080116         reso_MGS = tasMGS;
026700080116         reso_LNA = tasLNA;
026800080116         reso_FBR = tasFBR;
026900080116         reso_CCA = tasCCA;
027000080116         reso_TBL = tasTBL;
027100080116         reso_CBO = tasCBO;
027200080116
027300080116        //?  trova i legami con la Mamma
027400080116         CHAIN (tasAAS:tasLNP:tasNRS:tasNSP) fnLBL01L;
027500080116
027600080116         IF %FOUND(fnLBL01L);
027700080116            fnLBL_Trovato = 'S';
027800080116            CHAIN (lblAAP:lblLPP:lblNRP:lblNSP) tiTAS30c;
027900080116
028000080116            IF %FOUND(tiTAS30c);
028100080116              TiTAS_Trovato = 'S';
028200080116              mamma_Ann = MAM_tasAAS;
028300080116              mamma_LNP = MAM_tasLNP;
028400080116              mamma_NRS = MAM_tasNRS;
028500080116              mamma_NSP = MAM_tasNSP;
028600080116              mamma_MGS = MAM_tasMGS;
028700080116              mamma_LNA = MAM_tasLNA;
028800080116              mamma_CCA = MAM_tasCCA;
028900080116              mamma_TBL = MAM_tasTBL;
029000080116              mamma_CBO = MAM_tasCBO;
029100080116            ENDIF;
029200080116
029300080111         ENDIF;
029400080111
029500080116
029600080116         write tnvrDPD2;
029700080111
029800080111
029900080111       ENDSR;
030000080111       //*******************************************************************************************
030100080111      /END-FREE
