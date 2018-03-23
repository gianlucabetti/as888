000100161118      *------------------------------------------------------------------*
000200161118      * Avviso al destinatario di affidamento spedizione - Check Allegati
000300161118      *------------------------------------------------------------------*
000400110617
000500161118     H decedit('0,') datedit(*dmy/) option(*nodebugio)
000600161118     h dftactgrp(*no) actgrp(*caller) bnddir('TRBM':'UBBNDDIR')
000700110617
000800110617      *---------------------------------------------------------------*
000900110617
001000161118     Ffiar801l  if   e           k disk
001100140725
001200161118     D dar8§pdf      e ds                  inz
001300161118     D wOK             s               n   inz
001400141013
001500161118     D pPathOrig       s            120A
001600161118     D pPathCli        s                   like(§AR8PPDFP)
001700161118     D pNomePDF        s             61A
001800161118     D pCheckOnly      s              1A
001900161118     D pFileName       s            256A
002000161118     D pEsito          s              1A
002100161118
002200161118
002300161118     D FNLSPDFR1       PR
002400161118     D                                     extpgm('FNLSPDFR1')
002500161118     D pPathOrig                    120A
002600161118     D pPathCli                            like(§AR8PPDFP)
002700161118     D pNomePDF                      61A
002800161118     D pCheckOnly                     1A
002900161118     D pFileName                    256A
003000161118     D pEsito                         1A
003100110617
003200130406
003300110627     C     *ENTRY        PLIST
003400161118     C                   PARM                    iPATH           120
003500161118     C                   PARM                    iAAS              4 0
003600161118     C                   PARM                    iLNP              3 0
003700161118     C                   PARM                    iNRS              2 0
003800161118     C                   PARM                    iNSP              7 0
003900161118     C                   PARM                    oOK               1
004000130406
004100110617      /free
004200140806
004300161118       // Inizializzo parametro di output
004400161118       oOK = '0';
004500161118
004600161118       // Scorro tutto il FIAR8/§PDF della bolla corrente
004700161118       exsr sr_elaAR8;
004800161118
004900161118       // Valorizzo parametro di output
005000161118       oOK = wOK;
005100161118
005200161118       // Chiusura
005300161118       *inlr = *on;
005400140806
005500161118
005600130406
005700130406       //-------------------------------------------------------------*
005800161118       // Verifica allegati                                           *
005900161110       //-------------------------------------------------------------*
006000161110        Begsr sr_elaAR8;
006100130406
006200161118          // Inizializzo il flag di "presenza totale"
006300161118          wOK = *on;
006400140806
006500161118          // Quindi procedo con l'elaborazione degli allegati
006600161118          setll (iAAS:iLNP:iNRS:iNSP:'§PDF') fiar801l;
006700161118          reade (iAAS:iLNP:iNRS:iNSP:'§PDF') fiar801l;
006800130406
006900161118          // leggo TUTTO il fiar801l
007000161118          dow not %eof(fiar801l) AND wOK;
007100140806
007200161118              // Ridefinisco i dati relativi al PDF
007300161118              dar8§pdf = ar8UNI;
007400161118              pPathOrig = iPATH;
007500161118              pPathCli = §AR8PPDFP;
007600161118              pNomePDF = §AR8PPDFN;
007700140806
007800161118              // Verifico presenza e univocità del file da trattare
007900161118              pEsito = '0';
008000161118              exsr sr_findPDF;
008100140806
008200161118              // Se no OK
008300161118              if pEsito <> '0';
008400161118                 wOK = *off;     // PDF non presente (o non univoco)
008500161118              endif;
008600130406
008700161118              // Quindi prosegui con la lettura
008800161118              reade (iAAS:iLNP:iNRS:iNSP:'§PDF') fiar801l;
008900130406
009000161118          enddo;
009100130406
009200130406        endsr;
009300140808
009400161118
009500140808
009600140808       //-------------------------------------------------------------*
009700140808       // Richiamo driver "trova PDF"                                 *
009800140808       //-------------------------------------------------------------*
009900140808        Begsr sr_findPDF;
010000140808
010100140808            pFileName = *blanks;
010200161118            callP(e) FNLSPDFR1 (pPathOrig  :
010300140808                                pPathCli   :
010400140808                                pNomePDF   :
010500140808                                pCheckOnly :
010600140808                                pFileName  :
010700161118                                pEsito);
010800140808
010900140808        endsr;
011000131210
