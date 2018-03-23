000100171201     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) ACTGRP(QILE)
000200171201     H*PARMS BNDDIR(PJXBND PJCBND)
000300171201     H*PARMS CVTOPT(*NONE)
000400171201     H DECEDIT('0,') DATEDIT(*DMY.)
000500941130      *
000600171220      *            REPERIMENTO regola detrazione Iva Credito
000700941130      *
000800171220R275  * Questo PGM  è stato creato con il task R275 Detraz. IVA a credito  DL. 50/2017
000900171124      *
001000171204     F*----------------------------------------------------*
001100171220     F* Regole per detrazione IVA a Credito
001200171220     F* Soc/Reg/Libro/DtInVal Unique
001300171220     FSDGRIC01L IF   E           K DISK
001400171220     D*-------------
001500941130     D* Passaggio Parametri
001600941130     D KPJBA         E DS
001700941130     D*-------------
001800941130     D* Parametri in ricezione
001900171220     D SDGSI32DS     E DS
002000171204     D*-------------
002100171220     D $FineRIC        S              1
002200171220     D $TrovatoRIC     S              1
002300171220     D*---------
002400171220     D DataDs          DS
002500171220     D  AnnoDs                        4s 0
002600171220     D   Tratt1                       1A   Inz('-')
002700171220     D  MeseDs                        2s 0 Inz(01)
002800171220     D   Tratt2                       1A   Inz('-')
002900171220     D  GiornoDs                      2s 0 Inz(01)
003000941130     C*----------------------------------------------------*
003100941130     C*                MAIN LINE PROGRAM
003200941130     C*----------------------------------------------------*
003300941130     C*
003400941130     C                   EXSR      INZVAR
003500941130     C*
003600171221     C                   Exsr      RepRegDetr
003700941130     C*
003800941130     C                   EXSR      ENDPGM
003900941130     C*
004000941130     C/EJECT
004100941130     C************************************************************
004200941130     C* OPERAZIONI INIZIALI
004300941130     C************************************************************
004400941130     C     *INZSR        BEGSR
004500941130     C*
004600941130     C     *ENTRY        PLIST
004700171220     C                   PARM                    SDGSI32DS
004800941130     C*
004900941130     C                   ENDSR
005000941130     C/EJECT
005100941130     C************************************************************
005200941130     C* INIZIALIZZAZIONE VARIABILI
005300941130     C************************************************************
005400941130     C     INZVAR        BEGSR
005500941130     C*
005600171220     C                   eval      ERRI32      = '0'
005700171220     C                   eval      EXISTI32    = '0'
005800171220     C*
005900171220     C* pulisco i dati da recuperare
006000171220     C                   clear                   TpRicI32
006100171220     C                   clear                   DtLiqI32
006200171220     C                   clear                   Msg1I32
006300171221     C                   clear                   Msg2I32
006400171221     C*
006500171221     C                   ENDSR
006600171221     C/EJECT
006700171221     C************************************************************
006800171221     C* Reperisco le regole di detrazione iva a credito
006900171221     C* per il periodo inserito
007000171221     C************************************************************
007100171221     C     RepRegDetr    Begsr
007200171221      *
007300171221     C                   Eval      $FineRIC    = *Off
007400171221     C                   Eval      $TrovatoRic = *Off
007500171221      *
007600171221      * verifico  se esistono regole sul libro/registro passatp
007700171221     C*
007800171221     C                   eval      RICSocieta = SocietaI32
007900171221     C                   eval      RICTpReg = TpRegI32
008000171221     C                   eval      RICLibro = LibroI32
008100171221     C*
008200171221     C                   Extrct    DtAnnotI32:*y AnnoDs
008300171221     C                   Move      DataDs        RICDtInVal
008400171221     C*
008500171221     C     K04RIC01      Setll     SDGRIC01L
008600171221     C     K03RIC01      Reade     SDGRIC01L                              21
008700171221     C*
008800171221     C                   Eval      $FineRIC    = *In21
008900171221     C*
009000171221     C                   DoW       $FineRIC    = *Off   and
009100171221     C                             $TrovatoRIC = *Off
009200171221     C* La data rientra nei limiti della regola e se si tratta di
009300171221     C* una nota, la regola deve valere anche per essa.
009400171221     C                   If        DtAnnotI32 >= RICDtInVal and
009500171221     C                             DtAnnotI32 <= RICDtFiVal
009600171221     C                   Exsr      SelRic
009700171221     C                   Endif
009800171221     C*
009900171221     C     K03RIC01      READE     SDGRIC01L                              21
010000171221     C                   Eval      $FineRIC    = *In21
010100171221     C                   enddo
010200171221      *
010300171221     C                   Endsr
010400171221     C************************************************************
010500171221     C* Selesione della regola trovata in base ai parametri ricevuti
010600171221     C************************************************************
010700171221     C     SelRic        BEGSR
010800171221     C*
010900171228     C                   If        (FattI32 = *On and
011000171228     C                              RICFatt = *On) or
011001171228     C                             (NaccI32 = *On and
011002171228     C                              RICNacc = *On)
011100171221     C* Primo periodo
011200171221     C                   Select
011300171221     C*
011400171221     C                   When      RicTpRIc = '1' and
011500171221     C                             %subdt(DtDocI32:*Y) =
011600171221     C                             (%subdt(DtAnnotI32:*y) -1)
011700171221     C                   Eval      $TrovatoRIC = *On
011800171221     C                   Eval      ExistI32    = *On
011900171221     C* Riporto la data di liquidazione
012000171221     C                   Eval      DTLiqI32  = RICDtLiq
012100171221     C*
012200171221     C                   When      RicTpRic = '2'  and
012300171221     C                             %subdt(DtDocI32:*Y) <>
012400171221     C                             (%subdt(DtAnnotI32:*y) -1)
012500171221     C                   Eval      $TrovatoRIC = *On
012600171221     C                   Eval      ExistI32    = *On
012700171221     C                   Eval      MSG1I32     = RicMSG1
012800171221     C                   Eval      TpRicI32    = RicTpRIc
012900171221     C*
013000171221     C                   When      RicTpRic = '3' and
013100171222     C                             %subdt(DtDocI32:*y) <
013200171221     C                             %subdt(DtAnnotI32:*y)
013300171221     C                   Eval      $TrovatoRIC = *On
013400171221     C                   Eval      ExistI32    = *On
013500171221     C                   Eval      MSG2I32     = RicMSG2
013600171221     C                   Eval      TpRicI32    = RicTpRIc
013700171222     C                   Eval      CausIndI32  = RicCausInd
013800171221     C*
013900171221     C                   EndSl
014000171221     C*
014100171221     C                   Endif
014200171221     C*
014300171221     C                   ENDSR
014400171204     C************************************************************
014500171204     C* DEFINIZIONE CAMPI : KList,VARIABILI,CONTATORI,INDICI...
014600171204     C************************************************************
014700171204     C     DefCam        BEGSR
014800171204     C*
014900171221     C* SDGRIC01L
015000171221     C     K03RIC01      KLIST
015100171221     C                   KFLD                    RICSocieta
015200171221     C                   KFLD                    RICTpReg
015300171221     C                   KFLD                    RICLibro
015400171221     C* SDGRIC01L
015500171221     C     K04RIC01      KLIST
015600171221     C                   KFLD                    RICSocieta
015700171221     C                   KFLD                    RICTpReg
015800171221     C                   KFLD                    RICLibro
015900171221     C                   KFLD                    RICDtInVal
016000171204     C*
016100171204     C                   ENDSR
016200171204     C/EJECT
016300171204     C************************************************************
016400171204     C* FINE PROGRAMMA
016500171204     C************************************************************
016600171204     C     ENDPGM        BEGSR
016700171204     C*
016800171204     C                   SETON                                            LR
016900171204     C                   RETURN
017000171204     C*
017100171204     C                   ENDSR
017200171204     C/EJECT
