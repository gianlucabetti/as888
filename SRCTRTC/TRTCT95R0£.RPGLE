000100890921     H DECEDIT('0,') DATEDIT(*DMY.)
000200991027     H dftactgrp(*yes)
000300991027
000400050909     FTIVGDTMP  UF   E             DISK
000500050121     Ftivawwwt  O    E             DISK
000600140220     Ftrtct95w  O    E             DISK
000700991027
000800991027     D prmppt          s             50
000900991027     D prmesito        s              1
001000991027     D wrkesito        s                   like(prmesito)
001100140225
001200140225     d CED_Bart        C                   CONST('CEDAlert@Brt.it;')
001300140225      *----------------------------------------------------------------------*
001400991027
001500060804     C                   exsr      traduci
001600140225
001700060804     C                   seton                                        LR
001800991027
001900140220      *----------------------------------------------------------------------*
002000991027     C     traduci       BEGSR
002100991027     C*
002200060804     C                   read      TIVGDTMP
002300991027     C                   dow       not %eof
002400121023     C*
002500121023     C* Se nei parametri non è richiesta "elaborazione fittizia" procedo
002600121023     C                   if        %subst(prmppt:1:1) <> 'K'
002700050121     C*
002800050121     C* Inizializzazione campi e variabili d out
002900060804     C                   clear                   tivaw000
003000060804     C*
003100060804     C* Valorizzo il buffer del file d output
003200080130     C                   eval      vawDTA = vgdDTA
003300140225     C*
003400140225     C*  campi file di WRK HAGER (come un TIVGD)
003500140225     C                   eval        FLATREC   = vgdDTA
003600140225     C                   eval        w95TIP    = vgdTIP
003700140225     C                   eval        w95KSU    = vgdKSU
003800140225     C                   eval        w95TSC    = vgdTSC
003900140225     C                   eval        w95DAT    = vgdDAT
004000140225     C                   eval        w95PGM    = vgdPGM
004100140225     C                   eval        w95PRG    = vgdPRG
004200140225     C                   eval        w95CNT    = vgdCNT
004300140225     C                   eval        w95STO    = vgdSTO
004400140225     C                   eval        w95FLO    = vgdFLO
004500140225     C                   eval        w95ATTRIB = vgdATTRIB
004600140225     C                   WRITE     TRTCT95WR
004700140225     C*
004800050121     C                   WRITE     TIVAW000
004900121023     C*
005000121023     C                   endif
005100121023     C*
005200050909     C                   DELETE    TIVGD000
005300050909     C                   READ      TIVGDTMP
005400991027     C                   ENDdo
005500991027     C*
005600991027     C                   eval      wrkesito = '0'
005700991027     C*
005800910830     C                   ENDSR
005900060804
006000991027
006100991027      /TITLE Operazioni iniziali.
006200991027     C     *inzsr        BEGSR
006300991027     C*
006400991027     C     *ENTRY        PLIST
006500991027     C                   parm                    prmppt
006600991027     C     wrkesito      parm      wrkesito      prmesito
006700991027     C*
006800991027     C                   ENDSR
006900140225      * ?------------------------------------------------------------------ */
007000140225      *?      X non bloccare in nessun caso il traduttore CLIENTI
007100140225      * ?------------------------------------------------------------------ */
007200140225     C     *pssr         BEGSR
007300140225     C
007400140225     C                   eval      prmesito = '2'
007500140225     c                   exsr      invio_mail
007600140225     C                   ENDSR     '*CANCL'
007700140225     C
007800140225     c*==================================================================*
007900140225      * Manda un Msg x E-mail
008000140225     c*==================================================================*
008100140225     c     Invio_Mail    begsr
008200140225      *
008300140225     C*   Alert_mail : invio Mail a CEDAlert@Bartolini.it             *
008400140225     C* Inizializzo variabili
008500140225     C                   movel     *blanks       wrkEml          253
008600140225     C                   movel     *blanks       wrkMsg         5000
008700140225     C                   movel     *blanks       wrkOgg           44
008800140225     C* Valorizzo i campi della e-m@ail - indirizzo
008900140225     C                   eval      wrkEml = CED_BART
009000140225     C                   eval      wrkOgg ='Problemi DOWNLD HAGER EDI !!'
009100140225     C                   EVAL      wrkMSG = 'ATTENZIONE : traduzione EDI -
009200140225     C                             STATI << HAGER >> errato su DOWNLD. -
009300140225     C                             Controllare il Download del Cliente. >> '
009400140225     C                   call(e)   'TRTCT00R2'
009500140225     C                   parm                    wrkEml
009600140225     C                   parm                    wrkOgg
009700140225     C                   parm                    wrkMsg
009800140225     C*
009900140225     C                   ENDSR
010000140225      * ?------------------------------------------------------------------ */
