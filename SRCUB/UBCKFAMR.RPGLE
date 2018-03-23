000100170904     H OPTION(*SRCSTMT)
000200170904     H DECEDIT('0,') DATEDIT(*DMY.)
000300170904     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*CALLER)
000400160804
000500170904     D/COPY GAITRASRC/SRCPROTOPI,UBCKFAMR
000600170904     D/COPY GAITRASRC/SRCPROTOPI,UBBS10R
000700090521
000800170904     D savCOP1         s                   like(oCKFAMCOP) inz
000900170904     D savCOP2         s                   like(oCKFAMCOP) inz
001000090520
001100170904     C*
001200170904     C     *ENTRY        plist
001300170904     C                   parm                    iCKFAMDRF
001400170904     C                   parm                    iCKFAMTLE
001500170904     C                   parm                    iCKFAMCLI1
001600170904     C                   parm                    iCKFAMCLI2
001700170904     C                   parm                    oCKFAMesito
001800170904     C                   parm                    oCKFAMCOP
001900170511     C*
002000170904     C* Monitoraggio intero flusso
002100170904     C                   monitor
002200170904     C*
002300170904     C* Inizializzo esito chiamata ad errore generico
002400170904     C                   eval      oCKFAMesito = -1
002500170904     C*
002600170904     C* Controllo parametri di input
002700170904     C*
002800170904     C* Se data riferimento a zero => considero data corrente
002900170904     C                   if        iCKFAMDRF = *zeros
003000170904     C                   eval      iCKFAMDRF = %dec(%date() : *iso)
003100170904     C                   endif
003200170904     C*
003300170904     C* Se tipo legame non indicato => assumo 'WW' per default
003400170904     C                   if        iCKFAMTLE = *zeros
003500170904     C                   eval      iCKFAMTLE = 'WW'
003600170904     C                   endif
003700170904     C*
003800170904     C* Se i codici clienti da verificare non sono entrambi valorizzati => errore bloccante
003900170904     C                   if        iCKFAMCLI1 = *zeros OR
004000170904     C                             iCKFAMCLI2 = *zeros
004100170904     C                   eval      oCKFAMesito = -2
004200170904     C                   return
004300170904     C                   endif
004400170904     C*
004500170904     C* Inizializzazioni variabili di work
004600170904     C                   clear                   savCOP1
004700170904     C                   clear                   savCOP2
004800170904     C*
004900170904     C* Reperisco il padre del primo codice da verificare
005000170904     C                   eval      iBS10DRF  = iCKFAMDRF
005100170904     C                   eval      iBS10TLE  = iCKFAMTLE
005200170904     C                   eval      iBS10CLI  = iCKFAMCLI1
005300170509     C*
005400170904     C                   call      'UBBS10R'
005500170904     C                   parm                    iBS10DRF
005600170904     C                   parm                    iBS10TLE
005700170904     C                   parm                    iBS10CLI
005800170904     C                   parm                    oBS10Esito
005900170904     C                   parm                    oBS10COP
006000170904     C                   parm                    oBS10TIP
006100170904     C*
006200170904     C* Se esito chiamata OK altrimenti uscita con errore
006300170904     C                   if        oBS10Esito = *zeros
006400170904     C                   eval      savCOP1 = oBS10COP
006500170904     C                   else
006600170904     C                   return
006700170904     C                   endif
006800170904     C*
006900170904     C* Reperisco il padre del secondo codice da verificare
007000170904     C                   eval      iBS10DRF  = iCKFAMDRF
007100170904     C                   eval      iBS10TLE  = iCKFAMTLE
007200170904     C                   eval      iBS10CLI  = iCKFAMCLI2
007300170904     C*
007400170904     C                   call      'UBBS10R'
007500170904     C                   parm                    iBS10DRF
007600170904     C                   parm                    iBS10TLE
007700170904     C                   parm                    iBS10CLI
007800170904     C                   parm                    oBS10Esito
007900170904     C                   parm                    oBS10COP
008000170904     C                   parm                    oBS10TIP
008100170904     C*
008200170904     C* Se esito chiamata OK altrimenti uscita con errore
008300170904     C                   if        oBS10Esito = *zeros
008400170904     C                   eval      savCOP2 = oBS10COP
008500170904     C                   else
008600170904     C                   return
008700170904     C                   endif
008800170904     C*
008900170904     C* Infine verifico la "famigliarità" dei due codici
009000170904     C                   if        savCOP1 = savCOP2
009100170904     C                   eval      oCKFAMesito = 1
009200170904     C                   else
009300170904     C                   eval      oCKFAMesito = 0
009400170904     C                   endif
009500170904     C*
009600170904     C* Gestisco errore inatteso
009700170904     C                   on-error
009800170904     C
009900170904     C                   return
010000170904     C*
010100170904     C* Fine monitoraggio
010200170904     C                   endmon
010300170904     C*
010400170904     C                   return
