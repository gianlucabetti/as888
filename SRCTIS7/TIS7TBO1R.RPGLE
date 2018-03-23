000100890921     H DECEDIT('0,') DATEDIT(*DMY.)
000200991027     H dftactgrp(*yes)
000300991027
000400050909     FTIVGDTMP  UF   E             DISK
000500050121     Ftivawwwt  O    E             DISK
000600991027
000700151119     D FNVAOEADS     e ds                  inz prefix('I_')
000800151119     D FNVAOEAR      e ds                  inz prefix('O_')
000900991027     D prmppt          s             50
001000991027     D prmesito        s              1
001100151119     D wrkesito        s                   like(prmesito)
001200151119     D w1              s              1    inz(' ')
001300151119     D w2              s              2    inz('  ')
001400070907
001500070907
001600060804     C                   exsr      traduci
001700060804     C                   seton                                        LR
001800991027
001900060804
002000991027     C     traduci       BEGSR
002100991027     C*
002200060804     C                   read      TIVGDTMP
002300991027     C                   dow       not %eof
002400151119     C                   eval      FNVAOEADS = vgdDTA
002500050121     C*
002600050121     C* Inizializzazione campi e variabili d out
002700060804     C                   clear                   tivaw000
002800151119     C*
002900151119     C* Passo dalla DS di INPUT a quella di OUTPUT
003000151119     C                   eval      O_VAOATB   = I_VAOATB
003100151119     C                   eval      O_VAOPOE   = w1+%trim(%editc(I_VAOPOE:'X'))
003200151119     C                   eval      O_VAONSR   = w2+%trim(%editc(I_VAONSR:'X'))
003300151119     C                   eval      O_VAONOR   = w1+%trim(%editc(I_VAONOR:'X'))
003400151119     C                   eval      O_VAONRV   = w1+%trim(%editc(I_VAONRV:'X'))
003500151119     C                   eval      O_VAOTOR   = I_VAOTOR
003600151119     C                   eval      O_VAOTCO   = I_VAOTCO
003700151119     C                   eval      O_VAODAO   = w2+%trim(%editc(I_VAODAO:'X'))
003800151119     C                   eval      O_VAOOAO   = w2+%trim(%editc(I_VAOOAO:'X'))
003900151119     C                   eval      O_VAOCOR   = w2+%trim(%editc(I_VAOCOR:'X'))
004000151119     C                   eval      O_VAORSO   = I_VAORSO
004100151119     C                   eval      O_VAOINO   = I_VAOINO
004200151119     C                   eval      O_VAOCAO   = I_VAOCAO
004300151119     C                   eval      O_VAOLOO   = I_VAOLOO
004400151119     C                   eval      O_VAOPRO   = I_VAOPRO
004500151119     C                   eval      O_VAONAO   = I_VAONAO
004600151119     C                   eval      O_VAOCRA   = w2+%trim(%editc(I_VAOCRA:'X'))
004700151119     C                   eval      O_VAORSR   = I_VAORSR
004800151119     C                   eval      O_VAOINR   = I_VAOINR
004900151119     C                   eval      O_VAOCAR   = I_VAOCAR
005000151119     C                   eval      O_VAOLOR   = I_VAOLOR
005100151119     C                   eval      O_VAOPRR   = I_VAOPRR
005200151119     C                   eval      O_VAONAR   = I_VAONAR
005300151119     C                   eval      O_VAORER   = I_VAORER
005400151119     C                   eval      O_VAOTER   = I_VAOTER
005500151119     C                   eval      O_VAOCRC   = w2+%trim(%editc(I_VAOCRC:'X'))
005600151119     C                   eval      O_VAORSC   = I_VAORSC
005700151119     C                   eval      O_VAOINC   = I_VAOINC
005800151119     C                   eval      O_VAOCAC   = I_VAOCAC
005900151119     C                   eval      O_VAOLOC   = I_VAOLOC
006000151119     C                   eval      O_VAOPRC   = I_VAOPRC
006100151119     C                   eval      O_VAONAC   = I_VAONAC
006200151119     C                   eval      O_VAOFFD   = I_VAOFFD
006300151119     C                   eval      O_VAODAR   = w2+%trim(%editc(I_VAODAR:'X'))
006400151119     C                   eval      O_VAOORR   = w2+%trim(%editc(I_VAOORR:'X'))
006500151119     C                   eval      O_VAORMP   = I_VAORMP
006600151119     C                   eval      O_VAONAM   = I_VAONAM
006700151119     C                   eval      O_VAONCL   = w1+%trim(%editc(I_VAONCL:'X'))
006800151119     C                   eval      O_VAOPKG   = w1+%trim(
006900151119     C                                             %editw(I_VAOPKG:'0      , '))
007000151119     C                   eval      O_VAOVLM   = w1+%trim(
007100151119     C                                             %editw(I_VAOVLM:'0  ,   '))
007200151119     C                   eval      O_VAOBNC   = w1+%trim(%editc(I_VAOBNC:'X'))
007300151119     C                   eval      O_VAOBLC   = w1+%trim(%editc(I_VAOBLC:'X'))
007400151119     C                   eval      O_VAOATT   = w1+%trim(%editc(I_VAOATT:'X'))
007500151119     C                   eval      O_VAOMTC   = w1+%trim(%editc(I_VAOMTC:'X'))
007600151119     C                   eval      O_VAOPAG   = I_VAOPAG
007700151119     C                   eval      O_VAOKSC   = w1+%trim(%editc(I_VAOKSC:'X'))
007800151119     C                   eval      O_VAOCTR   = I_VAOCTR
007900151119     C                   eval      O_VAOPOR   = w1+%trim(%editc(I_VAOPOR:'X'))
008000151119     C                   eval      O_VAOZOR   = I_VAOZOR
008100151119     C                   eval      O_VAOPOC   = w1+%trim(%editc(I_VAOPOC:'X'))
008200151119     C                   eval      O_VAONO1   = I_VAONO1
008300151119     C                   eval      O_VAONO2   = I_VAONO2
008400151119     C                   eval      O_VAODDT   = I_VAODDT
008500151119     C                   eval      O_VAORFA   = I_VAORFA
008600151119     C                   eval      O_VAOSTO   = w1+%trim(%editc(I_VAOSTO:'X'))
008700151119     C                   eval      O_VAOSPI   = I_VAOSPI
008800151119     C                   eval      O_VAOFLO   = I_VAOFLO
008900151119     C                   eval      O_VAOFLO2  = I_VAOFLO2
009000151119     C                   eval      O_VAOTSPS  = I_VAOTSPS
009100151119     C                   eval      O_VAONOTE  = I_VAONOTE
009200151119     C                   eval      O_VAOORDA1 = w1+%trim(%editc(I_VAOORDA1:'X'))
009300151119     C                   eval      O_VAOORAA1 = w1+%trim(%editc(I_VAOORAA1:'X'))
009400151119     C                   eval      O_VAOORDA2 = w1+%trim(%editc(I_VAOORDA2:'X'))
009500151119     C                   eval      O_VAOORAA2 = w1+%trim(%editc(I_VAOORAA2:'X'))
009600151119     C                   eval      O_VAOEMLR  = I_VAOEMLR
009700151119     C                   eval      O_VAOEMLRF = I_VAOEMLRF
009800151119     C                   eval      O_VAOSMSR  = I_VAOSMSR
009900151119     C                   eval      O_VAOSMSRF = I_VAOSMSRF
010000151119     C                   eval      O_VAOORIP  = I_VAOORIP
010100151119     C                   eval      O_VAODOIO  = I_VAODOIO
010200151130     C*
010300151130     C* Forzature per NON-valorizzazione campi in uscita verso partner/cliente
010400151130     C                   clear                   O_VAOFLO
010500151130     C                   clear                   O_VAOFLO2
010600151130     C                   clear                   O_VAOORIP
010700151130     C                   clear                   O_VAODOIO
010800151119     C*
010900060804     C* Valorizzo il buffer del file d output
011000151130     C                   eval      vawdta = FNVAOEAR + '.'
011100930409     C*
011200151119     C                   write     TIVAW000
011300151119     C                   delete    TIVGD000
011400991027     C*
011500151119     C                   read      TIVGDTMP
011600151119     C                   enddo
011700991027     C*
011800991027     C                   eval      wrkesito = '0'
011900991027     C*
012000910830     C                   ENDSR
012100060804
012200151119
012300991027
012400991027      /TITLE Operazioni iniziali.
012500991027     C     *inzsr        BEGSR
012600991027     C*
012700991027     C     *ENTRY        PLIST
012800991027     C                   parm                    prmppt
012900991027     C     wrkesito      parm      wrkesito      prmesito
013000101129     C*
013100991027     C                   ENDSR
