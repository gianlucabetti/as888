000100091019     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PJXBND) ACTGRP(QILE)
000200091019     H*PARMS CVTOPT(*NONE)
000300091019     H DECEDIT('0,') DATEDIT(*DMY.)
000400940316      *
000401130925      *-------------------------------------------------------------------
000402130925      *-------------------------------------------------------------------
000403130925      * Controlla se per il quadro esistono già delle righe
000404130925      *
000405130925      * CONTA ANCHE GLI ANNULLATI
000406130925      *
000407130925      * Parametri di OUTPUT:
000408130925      *    ERRG78         1A      Flag di ritorno errore
000409130925      *    NrRcdG78       9S 0    Numero di record esistenti nel quadro
000410130925      *    NrOpeG78       1A      Dice se nel record è valorizzato
000411130925      *                           il numero delle operazioni aggregate
000412130925      *
000413130925      *-------------------------------------------------------------------
000500091019     F*------------
000600130925     F* Spesometro - Movimenti estratti
000800130925     F* Key: Soc/TpCom/Anno/Dati di raggruppam. FIFO
000900130925     FSDGSCF02L IF   E           K DISK
001000091019     D*------------
001100091019     D* DS per il passaggio dei parametri
001200130925     D SDGSG78DS     E DS
001300091019     D*-------------
001400091019     D* Passaggio Parametri
001500091019     D KPJBA         E DS
001501130925     D*-------------
001502130925     D* Campi di lavoro
001503130925     D $FineSCF        S              1A
002501130925     C*----------------------------------------------------*
002502130925     C*                MAIN LINE PROGRAM
002503130925     C*----------------------------------------------------*
002504130925      *
002600940330     C     *ENTRY        PLIST
002700091019     C                   PARM                    KPJBA
002701130925     C                   PARM                    SDGSG78DS
002901130925      *
003000130925     C                   Eval      ERRG78      = *Off
003001130925     C                   Eval      NrRcdG78    = 0
003002130925     C                   Eval      NrOpeG78    = *Off
003100091019      *
003200130925      * SDGSCF02L
004000130925     C     K24SCF02      Klist
004100130925     C                   Kfld                    SCFSOCIETA
004200130925     C                   Kfld                    SCFTPCOM
004300130925     C                   Kfld                    SCFANNO
004400130925     C                   Kfld                    SCFMESE
004501130925     C                   Kfld                    SCFSEQQUA
004502130925     C                   Kfld                    SCFQUADRO
004503130925     C                   Kfld                    SCFCOGRAGR
004504130925     C                   Kfld                    SCFNOMER
004505130925     C                   Kfld                    SCFDTNASR
004506130925     C                   Kfld                    SCFLOCNASR
004507130925     C                   Kfld                    SCFPRVNASR
004508130925     C                   Kfld                    SCFSTATOR
004509130925     C                   Kfld                    SCFCODUICR
004510130925     C                   Kfld                    SCFLOCALIR
004511130925     C                   Kfld                    SCFINDIRER
004600130925     C                   Kfld                    SCFTPNR
004601130925     C                   Kfld                    SCFPIVAR
004602130925     C                   Kfld                    SCFCDFISCR
004603130925     C                   Kfld                    SCFTPOPER
004604130925     C                   Kfld                    SCFNATOPER
004605130925     C                   Kfld                    SCFLEASINR
004606130925     C                   Kfld                    SCFDTREGR
004607130925     C                   Kfld                    SCFNRREGR
004608130925     C                   Kfld                    SCFSERREGR
004700130925     C*
005200091019     C*
005300130925     C                   Eval      SCFSOCIETA  = SOCIETAG78
005301130925     C                   Eval      SCFTPCOM    = TPCOMG78
005302130925     C                   Eval      SCFANNO     = ANNOG78
005303130925     C                   Eval      SCFMESE     = MESEG78
005304130925     C                   Eval      SCFSEQQUA   = SEQQUAG78
005305130925     C                   Eval      SCFQUADRO   = QUADROG78
005306130925     C                   Eval      SCFCOGRAGR  = COGRAGRG78
005307130925     C                   Eval      SCFNOMER    = NOMERG78
005308130925     C                   Eval      SCFDTNASR   = DTNASRG78
005309130925     C                   Eval      SCFLOCNASR  = LOCNASRG78
005310130925     C                   Eval      SCFPRVNASR  = PRVNASRG78
005311130925     C                   Eval      SCFSTATOR   = STATORG78
005312130925     C                   Eval      SCFCODUICR  = CODUICRG78
005313130925     C                   Eval      SCFLOCALIR  = LOCALIRG78
005314130925     C                   Eval      SCFINDIRER  = INDIRERG78
005315130925     C                   Eval      SCFTPNR     = TPNRG78
005316130925     C                   Eval      SCFPIVAR    = PIVARG78
005317130925     C                   Eval      SCFCDFISCR  = CDFISCRG78
005318130925     C                   Eval      SCFTPOPER   = TPOPERG78
005319130925     C                   Eval      SCFNATOPER  = NATOPERG78
005320130925     C                   Eval      SCFLEASINR  = LEASINRG78
005321130925     C                   Eval      SCFDTREGR   = DTREGRG78
005322130925     C                   Eval      SCFNRREGR   = NRREGRG78
005323130925     C                   Eval      SCFSERREGR  = SERREGRG78
005347130925     C*
005348130925     C     K24SCF02      SetLL     SDGSCF02L
005349130925     C     K24SCF02      READE     SDGSCF02L                              21
005350130925     C                   Eval      $FineSCF    = *In21
005351130925     C*
005352130925     C                   DoW       $FineSCF    = *Off
005353130925     C*
005354130925     C* mi assicuro di non contare lo stesso record
005355130925     C                   if        SCFSys     <> SYSG78      or
005356130925     C                             SCFNRASREG <> NRASREGG78  or
005357130925     C                             SCFNRRIGAM <> NRRIGAMG78  or
005358130925     C                             SCFNRRIGA  <> NRRIGAG78
005359130925     C*
005360130925     C                   Add       1             NrRcdG78
005361130925     C*
005362130925     C* verifico se il numero delle operazioni è impostato
005364130925     C                   if        SCFNrOpe    > 0
005366130925     C                   Eval      NrOpeG78    = *On
005367130925     C                   endif
005368130925     C*
005369130925     C                   endif
005370130925     C*
005371130925     C* esco dal ciclo, mi basta sapere se c'è
005372130925     C* anche solo un record
005373130925     C                   if        NrRcdG78    = 2
005374130925     C                   LEAVE
005375130925     C                   endif
005376130925     C*
005377130925     C     K24SCF02      READE     SDGSCF02L                              21
005378130925     C                   Eval      $FineSCF    = *In21
005379130925     C                   endDo
005700091019     C*
007500091019     C*
007600130925     C                   MoveL     SDGSG78DS     KPJBU
007700091019     C*
007701130925     C                   SetOn                                            RT
007702130925     C                   RETURN
007900091019     C*
