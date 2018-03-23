000100110617      *---------------------------------------------------------------*
000200161220      * Crea spool da PDF allegati tariffari fissi                    *
000300110617      *---------------------------------------------------------------*
000400110617
000500161220     H decedit('0,') datedit(*dmy/) option(*nodebugio)
000600161221     H dftactgrp(*no) actgrp(*caller)
000700110617
000800110617      *---------------------------------------------------------------*
000900161118
001000161220
001100161220     D*********
001200161220     D* STATUS DS
001300161220     D*********
001400161220     D                sds
001500161220     D  SDSpgm           *proc
001600161220
001700161220
001800161220     D*********
001900161220     D* DS DI PROCEDURA
002000161220     D*********
002100161220     D PdfTabel      e ds
002200161220     D TRBMATRCDS      ds                  qualified inz
002300161220     D   PdfPath                    128A
002400161220     D   PdfName                     64A
002500161220     D   WorkPath                   128A
002600161220     D   Device                      20A
002700161220     D   Resolution                   4A
002800161221     D   UsrData                     10A
002900161220     D   FormType                    10A
003000161221     D   Printer                     10A
003100161220     D   EsitoOK                      1A
003200161220
003300161220
003400161220     D*********
003500161220     D* VARIABILI DI WRK
003600161220     D*********
003700161220     D pTBKEY          s                   like(TBKEY)
003800161220     D pTBELE          s                   like(TBELE)
003900161220     D pPGM            s             10A
004000161220     D pTBDAT          s                   like(TBDAT)
004100161220     D wErrorGlobal    s              1A   inz
004200161221     D wEsito          s              1A   inz
004300161220
004400161220
004500161220     D*********
004600161220     D* PROTOTIPI CHIAMATE ESTERNE
004700161220     D*********
004800161220     D PDF100R         PR
004900161220     D                                     extpgm('PDF100R')
005000161220     D iTBKEY                              like(TBKEY)
005100161220     D iTBELE                              like(TBELE)
005200161220     D iPGM                          10A
005300161220     D iTBDAT                              like(TBDAT)
005400161220     D iEsito                         1A
005500161220     D*
005600161220     D TRBMATRC        PR
005700161220     D                                     extpgm('TRBMATRC')
005800161220     D PdfPath                             like(TRBMATRCDS.PdfPath)
005900161220     D PdfName                             like(TRBMATRCDS.PdfName)
006000161220     D WorkPath                            like(TRBMATRCDS.WorkPath)
006100161220     D Device                              like(TRBMATRCDS.Device)
006200161220     D Resolution                          like(TRBMATRCDS.Resolution)
006300161221     D UsrData                             like(TRBMATRCDS.UsrData)
006400161220     D FormType                            like(TRBMATRCDS.FormType)
006500161221     D Printer                             like(TRBMATRCDS.Printer)
006600161220     D EsitoOK                             like(TRBMATRCDS.EsitoOK)
006700161220     D*
006800110617
006900161220
007000161221
007100161221     C     *ENTRY        PLIST
007200161221     C                   PARM                    piPdfPath       128
007300161221     C                   PARM                    piPdfName        64
007400161221     C                   PARM                    piUsrData        10
007500161221     C                   PARM                    piFormType       10
007600161221     C                   PARM                    piPrinter        10
007700161221     C                   PARM                    poEsitoOK         1
007800161221
007900161221      /free
008000161220
008100161220       // Inizializzo flag di errore globale procedura
008200161220       wErrorGlobal = '0';
008300161220
008400161220       // Monitorizzo l'intero processo
008500161220       Monitor;
008600161220
008700161220          // Carico le tabelle occorrenti
008800161220          exsr cartab;
008900161220
009000161221          // Eseguo stampa allegato richiesto
009100161221          exsr exeSPOOL;
009200161220
009300161220      // Gestione errore
009400161220       On-Error;
009500161220          wErrorGlobal = '1';
009600161220          exsr uscita;
009700140725
009800161220       // Fine monitoraggio
009900161220       EndMon;
010000161220
010100161220       // Uscita
010200161220       exsr uscita;
010300161220
010400161220
010500140725
010600161220       //-------------------------------------------------------------*
010700161220       // Caricamento dati tabellari                                  *
010800161220       //-------------------------------------------------------------*
010900161220       BEGSR cartab;
011000161220
011100161220          // Reperisco i parametri di procedura da PDFTABEL
011200161220          pTBKEY = 'DDT';
011300161220          pTBELE = 'WORKPATH';
011400161220          pPGM   = SDSpgm;
011500161220          pTBDAT = *blanks;
011600161220          callP(e) PDF100R (pTBKEY :
011700161220                            pTBELE :
011800161220                            pPGM   :
011900161220                            pTBDAT :
012000161220                            wEsito);
012100161220          if %error or wEsito <> '0';
012200161220             wErrorGlobal = '1';
012300161220          else;
012400161220             TRBMATRCDS.WorkPath = %trim(pTBDAT);
012500161220          endif;
012600161220
012700161220          // Reperisco i parametri di procedura da PDFTABEL
012800161220          pTBKEY = 'DDT';
012900161220          pTBELE = 'PCLRESOLUTIONH';
013000161220          pPGM   = SDSpgm;
013100161220          pTBDAT = *blanks;
013200161220          callP(e) PDF100R (pTBKEY :
013300161220                            pTBELE :
013400161220                            pPGM   :
013500161220                            pTBDAT :
013600161220                            wEsito);
013700161220          if %error or wEsito <> '0';
013800161220             wErrorGlobal = '1';
013900161220          else;
014000161220             TRBMATRCDS.Resolution = %trim(pTBDAT);
014100161220          endif;
014200161220
014300161220          // Reperisco i parametri di procedura da PDFTABEL
014400161220          pTBKEY = 'DDT';
014500161220          pTBELE = 'PCLDEVICE';
014600161220          pPGM   = SDSpgm;
014700161220          pTBDAT = *blanks;
014800161220          callP(e) PDF100R (pTBKEY :
014900161220                            pTBELE :
015000161220                            pPGM   :
015100161220                            pTBDAT :
015200161220                            wEsito);
015300161220          if %error or wEsito <> '0';
015400161220             wErrorGlobal = '1';
015500161220          else;
015600161220             TRBMATRCDS.Device = %trim(pTBDAT);
015700161220          endif;
015800161220
015900161220          // Se NO tutti dati tabellati OK => esco con errore
016000161220          if wErrorGlobal = *on;
016100161220             exsr uscita;
016200161220          endif;
016300130406
016400161220       ENDSR;
016500140808
016600161220
016700140808
016800140808       //-------------------------------------------------------------*
016900161220       // Effettuo la stampa da PDF a SPOOL del allegato corrente     *
017000140808       //-------------------------------------------------------------*
017100161220        BEGSR exeSPOOL;
017200161220
017300161221            TRBMATRCDS.PdfPath  = piPdfPath;
017400161221            TRBMATRCDS.PdfName  = piPdfName;
017500161221            TRBMATRCDS.UsrData  = piUsrData;
017600161221            TRBMATRCDS.FormType = piFormType;
017700161221            TRBMATRCDS.Printer  = piPrinter;
017800161221
017900161220            callP(e) TRBMATRC (TRBMATRCDS.PdfPath
018000161220                              :TRBMATRCDS.PdfName
018100161220                              :TRBMATRCDS.WorkPath
018200161220                              :TRBMATRCDS.Device
018300161220                              :TRBMATRCDS.Resolution
018400161221                              :TRBMATRCDS.UsrData
018500161220                              :TRBMATRCDS.FormType
018600161221                              :TRBMATRCDS.Printer
018700161220                              :TRBMATRCDS.EsitoOK
018800161220                              );
018900161220
019000161220            if %error OR TRBMATRCDS.EsitoOK = *off;
019100161220               wErrorGlobal = '1';
019200161220               exsr uscita;
019300161220            endif;
019400140808
019500161220        ENDSR;
019600161220
019700161220
019800161220
019900161220       //-------------------------------------------------------------*
020000161220       // Uscita incondizionata                                       *
020100161220       //-------------------------------------------------------------*
020200161220        BEGSR uscita;
020300161220
020400161220           *inlr = *on;
020500161220           return;
020600161220
020700161220        ENDSR;
020800131210
