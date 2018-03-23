000100890921     H DECEDIT('0,') DATEDIT(*DMY.)
000200991027     H dftactgrp(*yes)
000300991027
000400050909     FTIVGDTMP  UF   E             DISK
000500050121     Ftivawwwt  O    E             DISK
000600991027
000700991027     D prmppt          s             50
000800991027     D prmesito        s              1
000900991027     D wrkesito        s                   like(prmesito)
001000140708
001100140708     D*-------------------
001200140708     D* Variabili d wrk x la chiamata al *pgm esterno d "bonifica" dati
001300140708     D*-------------------
001400140708     D TxtInOut        S           2048
001500140708     D ElencoChar      S            256
001600140708     D TipoElenco      S              1
001700140708     D CharSost        S              1
001800140708     D UpperCase       S              1
001900140708     D ChkNull         S              1
002000140708     D CharNull        S              1
002100140708     D Esito           S              1
002200140708
002300140708
002400991027
002500060804     C                   exsr      traduci
002600060804     C                   seton                                        LR
002700140708
002800140708
002900140708      /TITLE "Bonifica" dati
003000140708     C     exeBon        BEGSR
003100140708     C*
003200140708     C* Imposto i parametri x il *pgm d "bonifica"
003300140708     C                   EVAL      ElencoChar = 'QWERTYUIOPASDFGHJKLZXCVBNMqwe'+
003400140708     C                                          'rtyuiopasdfghjklzxcvbnm'      +
003500140708     C                                          '1234567890.,;:-_/\()!|$'      +
003600140708     C                                          '%&=?+*@<>''"'
003700140708     C                   EVAL      TipoElenco = *blanks
003800140708     C                   EVAL      CharSost   = *blanks
003900140708     C                   EVAL      UpperCase  = *blanks
004000140708     C                   EVAL      ChkNull    = '1'
004100140708     C                   EVAL      CharNull   = *blanks
004200140708     C                   EVAL      Esito      = *blanks
004300140708     C*
004400140708     C* Effettuo la chiamata al *pgm d "bonifica"
004500140708     C                   CALL      'XCHKCHAR'
004600140708     C                   PARM                    TxtInOut
004700140708     C                   PARM                    ElencoChar
004800140708     C                   PARM                    TipoElenco
004900140708     C                   PARM                    CharSost
005000140708     C                   PARM                    UpperCase
005100140708     C                   PARM                    ChkNull
005200140708     C                   PARM                    CharNull
005300140708     C                   PARM                    Esito
005400140708     C*
005500140708     C                   ENDSR
005600991027
005700060804
005800991027     C     traduci       BEGSR
005900991027     C*
006000060804     C                   read      TIVGDTMP
006100991027     C                   dow       not %eof
006200121023     C*
006300121023     C* Se nei parametri non è richiesta "elaborazione fittizia" procedo
006400121023     C                   if        %subst(prmppt:1:1) <> 'K'
006500050121     C*
006600050121     C* Inizializzazione campi e variabili d out
006700060804     C                   clear                   tivaw000
006800140708     C*
006900140708     C* Bonifico il buffer d output da caratteri NON consentiti
007000140708     C                   EVAL      TxtInOut = vgdDTA
007100140708     C                   EXSR      exeBon
007200140708     C                   eval      vawDTA = TxtInOut
007300930409     C*
007400050121     C                   WRITE     TIVAW000
007500121023     C*
007600121023     C                   endif
007700121023     C*
007800050909     C                   DELETE    TIVGD000
007900050909     C                   READ      TIVGDTMP
008000991027     C                   ENDdo
008100991027     C*
008200991027     C                   eval      wrkesito = '0'
008300991027     C*
008400910830     C                   ENDSR
008500060804
008600991027
008700991027      /TITLE Operazioni iniziali.
008800991027     C     *inzsr        BEGSR
008900991027     C*
009000991027     C     *ENTRY        PLIST
009100991027     C                   parm                    prmppt
009200991027     C     wrkesito      parm      wrkesito      prmesito
009300991027     C*
009400991027     C                   ENDSR
