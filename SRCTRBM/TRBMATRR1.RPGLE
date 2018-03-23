000100110617      *---------------------------------------------------------------*
000200161220      * Crea spool da PDF allegati tariffari fissi                    *
000300110617      *---------------------------------------------------------------*
000400110617
000500161220     H decedit('0,') datedit(*dmy/) option(*nodebugio)
000600161220     H dftactgrp(*no) actgrp(*caller) bnddir('UBBNDDIR')
000700110617
000800110617      *---------------------------------------------------------------*
000900110617
001000161220     Ftimara3i  if   e           k Disk
001100161221
001200161221
001300161221     D*********
001400161221     D* STATUS DS
001500161221     D*********
001600161221     D                sds
001700161221     D  SDSpgm           *proc
001800161220
001900161221
002000161221     D*********
002100161221     D* DS DI PROCEDURA
002200161221     D*********
002300161221     D PdfTabel      e ds
002400161221     D TRBMATRRDS      ds                  qualified inz
002500161221     D   PdfPath                    128A
002600161221     D   PdfName                     64A
002700161221     D   UsrData                     10A
002800161221     D   FormType                    10A
002900161221     D   Printer                     10A
003000161221     D   EsitoOK                      1A
003100161221
003200161220
003300161220     D*********
003400161220     D* VARIABILI DI WRK
003500161220     D*********
003600161221     D pTBKEY          s                   like(TBKEY)
003700161221     D pTBELE          s                   like(TBELE)
003800161221     D pPGM            s             10A
003900161221     D pTBDAT          s                   like(TBDAT)
004000161220     D wErrorGlobal    s              1A   inz
004100161221     D wEsito          s              1A   inz
004200161220
004300110617
004400161220     D*********
004500161220     D* IMPORTAZIONE DEFINIZIONI ESTERNE
004600161220     D*********
004700161220     D/COPY GAITRASRC/SRCPROTOPI,UBIFSUTIR
004800161220     D/COPY GAITRASRC/SRCPROTOPR,UBIFSUTIR
004900161220
005000161220
005100161221     D*********
005200161221     D* PROTOTIPI CHIAMATE ESTERNE
005300161221     D*********
005400161221     D PDF100R         PR
005500161221     D                                     extpgm('PDF100R')
005600161221     D iTBKEY                              like(TBKEY)
005700161221     D iTBELE                              like(TBELE)
005800161221     D iPGM                          10A
005900161221     D iTBDAT                              like(TBDAT)
006000161221     D iEsito                         1A
006100161221     D*
006200161221     D TRBMATRR        PR
006300161221     D                                     extpgm('TRBMATRR')
006400161221     D PdfPath                             like(TRBMATRRDS.PdfPath)
006500161221     D PdfName                             like(TRBMATRRDS.PdfName)
006600161221     D UsrData                             like(TRBMATRRDS.UsrData)
006700161221     D FormType                            like(TRBMATRRDS.FormType)
006800161221     D Printer                             like(TRBMATRRDS.Printer)
006900161221     D EsitoOK                             like(TRBMATRRDS.EsitoOK)
007000161220     D*
007100110617
007200161220
007300130406
007400110627     C     *ENTRY        PLIST
007500161220     C                   PARM                    piCodRegola       3
007600161220     C                   PARM                    piCodOperat      15
007700161221     C                   PARM                    piUserData       10
007800161221     C                   PARM                    piTipoModulo     10
007900161221     C                   PARM                    piPrinter        10
008000161221     C                   PARM                    poEsitoOK         1
008100130406
008200110617      /free
008300161220
008400161221       // Inizializzo parametri di output
008500161221       poEsitoOK = '0';
008600161221
008700161220       // Inizializzo flag di errore globale procedura
008800161220       wErrorGlobal = '0';
008900161220
009000161220       // Monitorizzo l'intero processo
009100161220       Monitor;
009200161221
009300161221          // Carico le tabelle occorrenti
009400161221          exsr cartab;
009500161220
009600161220          // Eseguo stampa allegati richiesti
009700161220          exsr exeStampa;
009800161220
009900161220      // Gestione errore
010000161220       On-Error;
010100161220          wErrorGlobal = '1';
010200161220          exsr uscita;
010300140725
010400161220       // Fine monitoraggio
010500161220       EndMon;
010600161220
010700161220       // Uscita
010800161221       poEsitoOK = '1';
010900161220       exsr uscita;
011000161221
011100161221
011200161221
011300161221       //-------------------------------------------------------------*
011400161221       // Caricamento dati tabellari                                  *
011500161221       //-------------------------------------------------------------*
011600161221       BEGSR cartab;
011700161221
011800161221          // Reperisco i parametri di procedura da PDFTABEL
011900161221          pTBKEY = 'BRT';
012000161221          pTBELE = 'PATHALLEGATI';
012100161221          pPGM   = SDSpgm;
012200161221          pTBDAT = *blanks;
012300161221          callP(e) PDF100R (pTBKEY :
012400161221                            pTBELE :
012500161221                            pPGM   :
012600161221                            pTBDAT :
012700161221                            wEsito);
012800161221          if %error or wEsito <> '0';
012900161221             wErrorGlobal = '1';
013000161221          else;
013100161221             TRBMATRRDS.PdfPath = %trim(pTBDAT);
013200161221          endif;
013300161221
013400161221          // Se NO tutti dati tabellati OK => esco con errore
013500161221          if wErrorGlobal = *on;
013600161221             exsr uscita;
013700161221          endif;
013800161221
013900161221       ENDSR;
014000161220
014100130406
014200130406
014300110617       //-------------------------------------------------------------*
014400161220       // Reperimento allegati da stampare richiesti                  *
014500110617       //-------------------------------------------------------------*
014600161220       BEGSR exeStampa;
014700161220
014800161220          // In relazioen al Codice Regola in input scorro tutti gli allegati da stampare
014900161220          setll (piCodOperat:piCodRegola) timara3i;
015000161220          reade (piCodOperat:piCodRegola) timara3i;
015100161220
015200161220          // leggo TUTTO il timara3i;
015300161220          dow not %eof(timara3i);
015400161220
015500161222              // Considero unicamente i record validi
015600170110              if maraATB <> 'A';
015700161222
015800161222                 // Distinguo gli elementi che compongono percorso/nome/estensione del allegato
015900161222                 exsr rtvFileNameElements;
016000161220
016100161222                 // Per ogni allegato genero quindi lo spool
016200161222                 exsr exeSPOOL;
016300161222
016400161222              endif;
016500161220
016600161220              // Proseguo nella lettura degli allegati
016700161220              reade (piCodOperat:piCodRegola) timara3i;
016800161220          enddo;
016900130406
017000110617        endsr;
017100161220
017200161220
017300161220
017400161220       //-------------------------------------------------------------*
017500161220       // Scoposizione elementi che compongono il full name del file  *
017600161220       //-------------------------------------------------------------*
017700161220       BEGSR rtvFileNameElements;
017800161220
017900161220          // Reperisco gli elementi del percorso/nome file
018000161220          if UBIFSUTIR_GetFileNameElements(%trim(maraVAL)
018100161220                                          :oIFSUTIpath
018200161220                                          :oIFSUTIfullName
018300161220                                          :oIFSUTIName
018400161220                                          :oIFSUTIExt) < 0;
018500161220             // Se errore/problema => esco con errore
018600161220             wErrorGlobal = '1';
018700161220             exsr uscita;
018800161220          endif;
018900161220
019000161220       ENDSR;
019100140808
019200161220
019300140808
019400140808       //-------------------------------------------------------------*
019500161220       // Effettuo la stampa da PDF a SPOOL del allegato corrente     *
019600140808       //-------------------------------------------------------------*
019700161220        BEGSR exeSPOOL;
019800161220
019900161221            TRBMATRRDS.PdfName  = oIFSUTIfullName;
020000161221            TRBMATRRDS.UsrData  = piUserData;
020100161221            TRBMATRRDS.FormType = piTipoModulo;
020200161221            TRBMATRRDS.Printer  = piPrinter;
020300161221            TRBMATRRDS.EsitoOK  = *blanks;
020400161221
020500161221            // Verifica valori parametri
020600161221            if TRBMATRRDS.UsrData  = *blanks;
020700161221               TRBMATRRDS.UsrData  = piCodRegola + %editc(maraSEQ:'X');
020800161221            endif;
020900161221            if TRBMATRRDS.Printer  = *blanks;
021000161221               TRBMATRRDS.Printer  = 'QPRINT';
021100161221            endif;
021200170110            if TRBMATRRDS.Printer  = '*DEV';
021300170110               TRBMATRRDS.Printer  = 'QPRINTS';
021400170110            endif;
021500161221            if TRBMATRRDS.FormType = *blanks;
021600161221               TRBMATRRDS.FormType = '*CARTAINT';
021700161221            endif;
021800161220
021900161221            callP(e) TRBMATRR (TRBMATRRDS.PdfPath
022000161221                              :TRBMATRRDS.PdfName
022100161221                              :TRBMATRRDS.UsrData
022200161221                              :TRBMATRRDS.FormType
022300161221                              :TRBMATRRDS.Printer
022400161221                              :TRBMATRRDS.EsitoOK
022500161220                              );
022600161220
022700161221            if %error OR TRBMATRRDS.EsitoOK = *off;
022800161220               wErrorGlobal = '1';
022900161220               exsr uscita;
023000161220            endif;
023100140808
023200161220        ENDSR;
023300161220
023400161220
023500161220
023600161220       //-------------------------------------------------------------*
023700161220       // Uscita incondizionata                                       *
023800161220       //-------------------------------------------------------------*
023900161220        BEGSR uscita;
024000161220
024100161220           *inlr = *on;
024200161220           return;
024300161220
024400161220        ENDSR;
024500131210
