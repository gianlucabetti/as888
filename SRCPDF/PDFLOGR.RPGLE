000100000000     H DFTACTGRP(*NO)
000200000000
000300000000      // Log
000400000000     FPDFLOG    O    E             DISK
000500000000
000600000000      // Parametri ------------------------------------------------------
000700000000     DPDFLOGR          PR                  ExtPgm('PDFLOG')
000800000000     D pLOGKEY                             like(LOGKEY)
000900000000     D pLOGDATA                            like(LOGDATA)
001000000000     D pLOGSEV                             like(LOGSEV)
001100000000     D pLOGMSG                             like(LOGMSG)
001200000000     D pLOGNUMPDF                          like(LOGNUMPDF)
001300000000     D pLOGNOPDF                           like(LOGNOPDF)
001400000000     D pOPTION                             like(OPTION)
001500000000     DPDFLOGR          PI
001600000000     D pLOGKEY                             like(LOGKEY)
001700000000     D pLOGDATA                            like(LOGDATA)
001800000000     D pLOGSEV                             like(LOGSEV)
001900000000     D pLOGMSG                             like(LOGMSG)
002000000000     D pLOGNUMPDF                          like(LOGNUMPDF)
002100000000     D pLOGNOPDF                           like(LOGNOPDF)
002200000000     D pOPTION                             like(OPTION)
002300000000
002400000000     D OPTION          S              6A
002500000000
002600000000      // Program status ---------------------------------------
002700000000     DPgmSts          SDS                  NoOpt
002800000000     D FullJob                       26A   Overlay(PgmSts:244)
002900000000
003000000000      //*******************************************************************
003100000000      // MAIN
003200000000      //*******************************************************************
003300000000
003400000000      /FREE
003500000000
003600000000       // Chiusura programma
003700000000       If (%parms = 0);
003800000000         *INLR = *ON;
003900000000         Return;
004000000000       EndIf;
004100000000
004200000000       // Opzione
004300000000       If (%parms > 6);
004400000000         OPTION = pOPTION;
004500000000       EndIf;
004600000000
004700000000       // Scrittura Log
004800000000       Clear RLOG;
004900000000       LOGKEY    = pLOGKEY;
005000000000       LOGDATA   = pLOGDATA;
005100000000       LOGDATE   = %date();
005200000000       LOGTIME   = %time();
005300000000       LOGJOB    = FullJob;
005400000000       LOGSEV    = pLOGSEV;
005500000000       LOGMSG    = pLOGMSG;
005600000000       If (%parms > 4);
005700000000         Monitor;
005800000000           LOGNUMPDF = pLOGNUMPDF;
005900000000           LOGNOPDF = pLOGNOPDF;
006000000000         On-Error;
006100000000         EndMon;
006200000000       EndIf;
006300000000       Write RLOG;
006400000000
006500000000       // Termine funzione
006600000000       *INLR = (OPTION = '*CLOSE');
006700000000       Return;
006800000000
006900000000      /END-FREE
