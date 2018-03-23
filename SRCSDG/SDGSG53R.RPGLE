000100091019     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PJXBND) ACTGRP(QILE)
000200091019     H*PARMS CVTOPT(*NONE)
000300091019     H DECEDIT('0,') DATEDIT(*DMY.)
000400940316      *
000401130925      *-------------------------------------------------------------------
000402130925      *-------------------------------------------------------------------
000403130926      * Reperisce i dati anagrafici del primo record inserito a parità
000404130926      * di partita IVA /Codice fiscale
000405130926      *
000413130925      *
000414130925      *-------------------------------------------------------------------
000500091019     F*------------
000600130925     F* Spesometro - Movimenti estratti
000800130926     F* Key: Soc/TpCom/Anno/PIva/CdFisc         FIFO
000900130926     FSDGSCF05L IF   E           K DISK
001000091019     D*------------
001100091019     D* DS per il passaggio dei parametri
001200130926     D SDGSG53DS     E DS
001300091019     D*-------------
001400091019     D* Passaggio Parametri
001500091019     D KPJBA         E DS
001501130925     D*-------------
001502130925     D* Campi di lavoro
001503130925     D $FineSCF        S              1A
001504130926     D SavSOCIETA      S                   Like(SCFSocieta)
001505130926     D SavTPCOM        S                   Like(SCFTpCom  )
001506130926     D SavANNO         S                   Like(SCFAnno   )
001507130926     D SavMESE         S                   Like(SCFMese   )
001508130926     D SavPartIVA      S                   Like(SCFPartIVA)
001509130926     D SavCdFisc       S                   Like(SCFCdFisc )
002501130925     C*----------------------------------------------------*
002502130925     C*                MAIN LINE PROGRAM
002503130925     C*----------------------------------------------------*
002504130926     C*
002600940330     C     *ENTRY        PLIST
002700091019     C                   PARM                    KPJBA
002701130926     C                   PARM                    SDGSG53DS
002901130926     C*
003002130926     C*
003003130926     C* se la partita IVA e codice fiscale sono gli stessi dell'ultima
003004130926     C* chiamata, ripasso i dati recuperati in precedenza
003005130926     C                   if        FoundG53    = *On         and
003006130926     C                             SocietaG53  = SavSOCIETA  and
003007130926     C                             TpComG53    = SavTPCOM    and
003008130926     C                             AnnoG53     = SavANNO     and
003009130926     C                             MeseG53     = SavMESE     and
003010130926     C                             PartIVAG53  = SavPartIVA  and
003011130926     C                             CdFiscG53   = SavCdFisc
003012130926     C*
003013130926     C                   MoveL     SDGSG53DS     KPJBU
003014130926     C                   SetOn                                            RT
003015130926     C                   RETURN
003016130926     C*
003017130926     C                   else
003018130926     C*
003019130926     C                   Eval      ERRG53      = *Off
003020130926     C                   Eval      FoundG53    = *Off
003021130926     C                   Eval      COGRAGG53   = *Blank
003022130926     C                   Eval      NOMEG53     = *Blank
003023130926     C                   Eval      DTNASCG53   = *Loval
003024130926     C                   Eval      LOCNASCG53  = *Blank
003025130926     C                   Eval      PRVNASCG53  = *Blank
003026130926     C                   Eval      STATOG53    = *Blank
003027130926     C                   Eval      CODUICG53   = 0
003028130926     C                   Eval      LOCALITG53  = *Blank
003029130926     C                   Eval      INDIREG53   = *Blank
003030130926     C*
003031130926     C                   endif
003100130926     C*
003200130926     C* SDGSCF05L
004000130926     C     K06SCF05      Klist
004100130925     C                   Kfld                    SCFSOCIETA
004200130925     C                   Kfld                    SCFTPCOM
004300130925     C                   Kfld                    SCFANNO
004400130925     C                   Kfld                    SCFMESE
004601130926     C                   Kfld                    SCFPartIVA
004602130926     C                   Kfld                    SCFCDFISC
004700130925     C*
005200091019     C*
005300130926     C                   Eval      SCFSOCIETA  = SOCIETAG53
005301130926     C                   Eval      SCFTPCOM    = TPCOMG53
005302130926     C                   Eval      SCFANNO     = ANNOG53
005303130926     C                   Eval      SCFMESE     = MESEG53
005316130926     C                   Eval      SCFPartIVA  = PartIVAG53
005317130926     C                   Eval      SCFCdFisc   = CdFiscG53
005347130925     C*
005348130926     C     K06SCF05      SetLL     SDGSCF05L
005349130926     C     K06SCF05      READE     SDGSCF05L                              21
005350130925     C                   Eval      $FineSCF    = *In21
005351130925     C*
005352130925     C                   DoW       $FineSCF    = *Off
005353130925     C*
005366130926     C                   Eval      COGRAGG53   = SCFCOGRAG
005367130926     C                   Eval      NOMEG53     = SCFNOME
005368130926     C                   Eval      DTNASCG53   = SCFDTNASC
005369130926     C                   Eval      LOCNASCG53  = SCFLOCNASC
005370130926     C                   Eval      PRVNASCG53  = SCFPRVNASC
005371130926     C                   Eval      STATOG53    = SCFSTATO
005372130926     C                   Eval      CODUICG53   = SCFCODUIC
005373130926     C                   Eval      LOCALITG53  = SCFLOCALIT
005374130926     C                   Eval      INDIREG53   = SCFINDIRE
005375130926     C*
005376130926     C                   Eval      FoundG53    = *On
005378130925     C*
005379130926     C* esco dal ciclo, mi basta recuperare i dati
005380130926     C* anagrafici del primo record trovato
005381130925     C                   LEAVE
005382130925     C*
005383130926     C     K06SCF05      READE     SDGSCF05L                              21
005384130925     C                   Eval      $FineSCF    = *In21
005385130925     C                   endDo
005700091019     C*
007500091019     C*
007600130926     C                   MoveL     SDGSG53DS     KPJBU
007700091019     C*
007701130925     C                   SetOn                                            RT
007702130925     C                   RETURN
007900091019     C*
