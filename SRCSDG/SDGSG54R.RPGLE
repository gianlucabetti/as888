000100160125     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PJXBND PJCBND)
000200160125     H*PARMS CVTOPT(*NONE) ACTGRP(QILE)
000300091019     H DECEDIT('0,') DATEDIT(*DMY.)
000400940316      *
000401130925      *-------------------------------------------------------------------
000402160118      * Nella comunicazione della Black List, le bolle devono essere
000403160118      * comunicate, mentre le fatture no.
000404160118      *
000405160118      *
000406160118      * Verifica la presenza del legame bolla doganale / fattura fornitore
000407160118      * Se presente:
000408160118      * - annullo la fattura perchè non deve essere comunicata
000409160118      * - annullo la bolla se riferita a fattura di anni precedenti, in
000410160118      *   quanto devono andare a modificare a mano l'importo della bolla
000411160118      *   lasciando l'importo delta con la fattura che si presuppone sia stata
000412160118      *   comunicata in precedenti comunicazioni
000413160118      *
000414160118      * Se sto controllando una bolla e il legame non è presente:
000415160118      * - annullo la bolla in modo che l'utente possa controllare cosa fare
000416160119      *
000417160119      * Se non trovo la registrazione a cui è legato il documento in esame,
000418160119      * annullo la fattura/bolla
000419130925      *
000420130925      *-------------------------------------------------------------------
000500091019     F*------------
000600160119     F* Legami registrazioni contabili
000800160119     F* Key: Soc/RegBolla     Unique
000900160125     FSDGLRC01L IF   E           K DISK    RENAME(SDGLRC000:SDGLRC01)
000902160119     F*------------
000903160119     F* Legami registrazioni contabili
000904160119     F* Key: Soc/RegFattura   Unique
000905160125     FSDGLRC02L IF   E           K DISK    RENAME(SDGLRC000:SDGLRC02)
001000091019     D*------------
001100091019     D* DS per il passaggio dei parametri
001200160119     D SDGSG54DS     E DS
001300091019     D*-------------
001400091019     D* Passaggio Parametri
001500091019     D KPJBA         E DS
001501160125     D*---------
001502160125     D* Ds ndmov
001503160125     D NDMOV00F      E DS
001506160125     D*---------
001507160125     D* Ds ndreg
001508160125     D NDREG00F      E DS
001511160125     D*---------
001512160125     D* Definizioni parametri standard chiamate moduli
001513160125     D $Sys            S                   Like(MOVSys)
001514160125     D $NrAsReg        S                   Like(MOVNrAsReg)
001515160125     D $NrRigaM        s                   like(MOVNrRigaM) inz
001516160125     D $Esito          S              1
001517160125     D $GesMsg         S              1
001518160125     D $LenOut         S              9B 0
001519160125     D $Struttura      S             10
001520160125     D $CodOper        S              1
001521160125     D $Output         S           4000
001522160125     D $Refresh        S              1
001523160125     D $Uso            S              1
001524130925     D*-------------
001525130925     D* Campi di lavoro
001526160125     D SavSOCIETA      S                   Like(LRCSocieta)
001527160125     D SavSys          S                   Like(LRCSys    )
001528160125     D SavNrAsReg      S                   Like(LRCNrAsReg)
001529160418R2212D $FineLRC        S              1
001530160418R2212D $FoundFat       S              1
002501130925     C*----------------------------------------------------*
002502130925     C*                MAIN LINE PROGRAM
002503130925     C*----------------------------------------------------*
002504130926     C*
002600940330     C     *ENTRY        PLIST
002700091019     C                   PARM                    KPJBA
002701160119     C*
002702160119     C                   MoveL     KPJBU         SDGSG54DS
002901130926     C*
003002130926     C*
003003160119     C* se il sys e il numero assoluto di registrazione
003004160119     C* sono gli stessi dell'ultima
003005130926     C* chiamata, ripasso i dati recuperati in precedenza
003007160119     C                   if        SocietaG54  = SavSOCIETA  and
003008160119     C                             SysG54      = SavSys      and
003009160119     C                             NrAsRegG54  = SavNrAsReg
003013130926     C*
003014160119     C                   MoveL     SDGSG54DS     KPJBU
003015130926     C                   SetOn                                            RT
003016130926     C                   RETURN
003017130926     C*
003018130926     C                   else
003019130926     C*
003022160119     C                   Eval      TpAnnG54    = *Blank
003023160119     C                   Eval      SoggG54     = *Blank
003024160129     C                   Eval      KccG54      = *Blank
003025160126     C                   Eval      KscG54      = *Blank
003031130926     C*
003032130926     C                   endif
004700130925     C*
004701160125     C                   Select
004702160125     C*
004703160125     C* Bolla doganale
004704160125     C                   When      TpDocG54    = 'B'
004705160125     C                   EXSR      ChainLRCB
004714160119     C*
004716160125     C* Fattura
004717160125     C                   When      TpDocG54    = 'F'
004718160125     C                   EXSR      ChainLRCF
004723160125     C                   endSl
007500091019     C*
007600160125     C                   MoveL     SDGSG54DS     KPJBU
007700091019     C*
007701130925     C                   SetOn                                            RT
007702130925     C                   RETURN
007703160125     C*
007706160119     C******************************************************
007707160125     C* Cerco il legame della bolla
007708160119     C******************************************************
007709160125     C     ChainLRCB     BEGSR
007717160119     C*
007718160418R2212C                   Eval      $FoundFat   = *Off
007719160418 "   C                   Z-Add     0             WAnnoB            4 0
007720160418 "   C                   Z-Add     0             WAnnoF            4 0
007721160418 "   C                   Z-Add     0             WAnnoGen          4 0
007722160418R2212C*
007723160129     C* con i riferimenti della bolla reperisco il soggetto
007724160129     C* dal legame
007725160125     C                   Eval      LRCSocieta  = SocietaG54
007726160125     C                   Eval      LRCSys      = SysG54
007727160125     C                   Eval      LRCNrAsReg  = NrAsRegG54
007728160125     C*
007729160125     C     K03LRC01      CHAIN     SDGLRC01L                          21
007730160418R2212C                   Eval      $FineLRC    = *In21
007731160125     C*
007732160418R2212X****               if        *In21       = *Off     and
007734160708R2322X*ex R2212          DoW       $FineLRC    = *Off     and
007735160708R2322X****                         LRCNrAsL   <> 0
007736160708R2322C*
007737160708R2322C                   DoW       $FineLRC    = *Off
007738160126     C*
007739160418R2212C                   Eval      $FoundFat   = *On
007740160126     C                   Eval      SoggG54     = LRCSogg
007741160126     C*
007742160708R2322C                   if        LRCNrAsL   <> 0
007743160129     C* con i riferimenti della fattura recuperati dal legame
007744160129     C* reperisco il conto del fornitore
007745160129     C                   Eval      $Sys        = LRCSysL
007746160129     C                   Eval      $NrAsReg    = LRCNrAsL
007747160126     C                   EXSR      GetNDMOV
007748160126     C                   if        $CodOper   <> '9'
007749160129     C                   Eval      KccG54      = MOVKcc
007750160126     C                   Eval      KscG54      = MOVKsc
007751160126     C                   endif
007753160126     C*
007754160418R2212X**** verifico se è fattura di anni precedenti
007755160418 "   X****
007756160418 "   X****               Z-Add     0             WAnnoB            4 0
007757160418 "   X****               Z-Add     0             WAnnoF            4 0
007758160418 "   X**** recupero l'anno della bolla doganale
007759160418 "   X****               Eval      $Sys        = LRCSys
007760160418 "   X****               Eval      $NrAsReg    = LRCNrAsReg
007761160418 "   X****               EXSR      GetNDREG
007762160418 "   X****               if        $CodOper   <> '9'
007763160418 "   X****               if        TpDtRifG54  = 'R'
007764160418 "   X****               Extrct    REGDtReg:*Y   WAnnoB
007765160418 "   X****               else
007766160418 "   X****               Extrct    REGDtDoc:*Y   WAnnoB
007767160418 "   X****               endif
007768160418R2212X****               endif
007769160125     C*
007770160125     C* recupero l'anno della fattura
007771160125     C                   Eval      $Sys        = LRCSysL
007772160125     C                   Eval      $NrAsReg    = LRCNrAsL
007773160125     C                   EXSR      GetNDREG
007774160125     C* non trovata registrazione della fattura
007775160125     C                   if        $CodOper    = '9'
007776160125     C                   Eval      TpAnnG54    = '55'
007777160125     C                   else
007778160125     C*
007779160125     C                   if        TpDtRifG54  = 'R'
007780160125     C                   Extrct    REGDtReg:*Y   WAnnoF
007781160125     C                   else
007782160125     C                   Extrct    REGDtDoc:*Y   WAnnoF
007783160125     C                   endif
007784160418R2212C*
007785160418 "   C* memorizzo l'anno minore valorizzato
007786160418 "   C                   if        WAnnoF      < WAnnoGen  or
007787160418 "   C                             WAnnoGen    = 0
007788160418 "   C                   Eval      WAnnoGen    = WAnnoF
007789160418 "   C                   endif
007790160418R2212C*
007791160125     C                   endif
007792160708R2322C                   endif
007793160418R2212C*
007794160418 "   C     K03LRC01      READE     SDGLRC01L                              21
007795160418 "   C                   Eval      $FineLRC    = *In21
007796160418R2212C                   endDO
007797160125     C*
007798160418R2212C                   if        $FoundFat   = *On
007799160418 "   C*
007800160418 "   C* recupero l'anno della bolla doganale
007801160418 "   C                   Eval      $Sys        = LRCSys
007802160418 "   C                   Eval      $NrAsReg    = LRCNrAsReg
007803160418 "   C                   EXSR      GetNDREG
007804160418 "   C                   if        $CodOper   <> '9'
007805160418 "   C                   if        TpDtRifG54  = 'R'
007806160418 "   C                   Extrct    REGDtReg:*Y   WAnnoB
007807160418 "   C                   else
007808160418 "   C                   Extrct    REGDtDoc:*Y   WAnnoB
007809160418 "   C                   endif
007810160418 "   C                   endif
007811160418R2212C*
007812160708R2322C                   if        LRCNrAsL   <> 0
007813160125     C*  Bolla collegata a fattura di anni precedenti
007814160418R2212X****               if        WAnnoF     <> WAnnoB
007815160418R2212C                   if        WAnnoGen    < WAnnoB
007816160125     C                   Eval      TpAnnG54    = '56'
007817160125     C                   endif
007818160708R2322C                   endif
007820160125     C*
007821160125     C                   else
007822160125     C* legame con fattura non trovato
007823160125     C                   Eval      TpAnnG54    = '57'
007824160125     C                   endif
008000160119     C*
008100160119     C                   ENDSR
008200160119     C/EJECT
008201160126     C************************************************************
008202160126     c* Reperisce dati del movimento
008203160126     C************************************************************
008204160126     C     GetNDMOV      BEGSR
008205160126     C*
008206160126     C                   Eval      $LenOut     = %size(NDMOV00F)
008207160126     C                   CALLB     'NDDRVMOV'
008208160126     C                   PARM                    $Sys
008209160126     C                   PARM                    $NrAsReg
008210160126     C* cerco la riga del cliente/fornitore (è sempre la num.1)
008211160126     C* quella dell'IVA è sempre la 3
008212160126     C                   PARM      1             $NrRigaM
008213160126     C                   PARM      'NDMOV     '  $Struttura
008214160126     C                   PARM                    $LenOut
008215160126     C                   PARM      '1'           $CodOper
008216160126     C                   PARM                    NDMOV00F
008217160126     C*
008218160126     C                   ENDSR
008219160126     C/EJECT
008300160125     C************************************************************
008400160125     c* Reperisce dati della registrazione
008500160125     C************************************************************
008600160125     C     GetNDREG      BEGSR
008700160125     C*
008800160125     C                   Eval      $LenOut     = %size(NDREG00F)
008900160125     C                   CALLB     'NDDRVREG'
009000160125     C                   PARM                    $Sys
009100160125     C                   PARM                    $NrAsReg
009200160125     C                   PARM      'NDREG     '  $Struttura
009300160125     C                   PARM                    $LenOut
009400160125     C                   PARM      '1'           $CodOper
009500160125     C                   PARM                    NDREG00F
009600160125     C*
009700160125     C                   ENDSR
009800160125     C/EJECT
009900160125     C******************************************************
010000160125     C* Cerco il legame della fattura
010100160125     C******************************************************
010200160125     C     ChainLRCF     BEGSR
010300160125     C*
010400160125     C                   Eval      LRCSocieta  = SocietaG54
010500160125     C                   Eval      LRCSysL     = SysG54
010600160125     C                   Eval      LRCNrAsL    = NrAsRegG54
010700160125     C*
010800160125     C     K03LRC02      CHAIN     SDGLRC02L                          21
010900160125     C                   if        *In21       = *Off
011000160125     C* verifico se è bolla dello stesso anno
011100160125     C*
011200160125     C                   Z-Add     0             WAnnoB            4 0
011300160125     C                   Z-Add     0             WAnnoF            4 0
011400160125     C* recupero l'anno della bolla doganale
011500160125     C                   Eval      $Sys        = LRCSys
011600160125     C                   Eval      $NrAsReg    = LRCNrAsReg
011700160125     C                   EXSR      GetNDREG
011701160125     C* non trovata registrazione della bolla
011800160125     C                   if        $CodOper    = '9'
011801160125     C                   Eval      TpAnnG54    = '58'
011802160125     C                   else
011803160125     C*
011900160125     C                   if        TpDtRifG54  = 'R'
012000160125     C                   Extrct    REGDtReg:*Y   WAnnoB
012100160125     C                   else
012200160125     C                   Extrct    REGDtDoc:*Y   WAnnoB
012300160125     C                   endif
012400160125     C                   endif
012500160125     C*
012600160125     C* recupero l'anno della fattura
012700160125     C                   Eval      $Sys        = LRCSysL
012800160125     C                   Eval      $NrAsReg    = LRCNrAsL
012900160125     C                   EXSR      GetNDREG
013000160125     C                   if        $CodOper   <> '9'
013100160125     C                   if        TpDtRifG54  = 'R'
013200160125     C                   Extrct    REGDtReg:*Y   WAnnoF
013300160125     C                   else
013400160125     C                   Extrct    REGDtDoc:*Y   WAnnoF
013500160125     C                   endif
013600160125     C                   endif
013700160125     C*
013701160125     C* fattura collegata a bolla registrata nello stesso anno
013800160125     C                   if        WAnnoF      = WAnnoB
013900160125     C                   Eval      TpAnnG54    = '59'
014000160125     C                   endif
014100160125     C*
014500160125     C                   endif
014600160125     C*
014700160125     C                   ENDSR
014800160125     C/EJECT
016104160125     C************************************************************
016105160125     C* Definizione campi
016106160125     C************************************************************
016107160125     C     DefKlist      BEGSR
016108160125     C*
016109160125     C*
016110160125     C* SDGLRC01L
016111160125     C     K03LRC01      Klist
016112160125     C                   Kfld                    LRCSOCIETA
016113160125     C                   Kfld                    LRCSys
016114160125     C                   Kfld                    LRCNrAsReg
016115160125     C*
016116160125     C* SDGLRC02L
016117160125     C     K03LRC02      Klist
016118160125     C                   Kfld                    LRCSOCIETA
016119160125     C                   Kfld                    LRCSysL
016120160125     C                   Kfld                    LRCNrAsL
016200160125     C*
016300160125     C                   ENDSR
016400160125     C/EJECT
