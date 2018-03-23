000100170726     H DECEDIT('0,') DATEDIT(*DMY.)
000200170726
000300170803     D/COPY GAITRASRC/SRCPROTOPI,UBDKC2BR
000400170803     D/COPY GAITRASRC/SRCPROTOPI,UBS717R
000500090521
000600170803     D DVPC          e ds                  inz
000700090520
000800170511     C*
000900170803     C                   eval      iDKC2BKSU  = '3250044'
001000170803     C                   eval      iDKC2BTLE  = *blanks
001100170803     C                   eval      iDKC2BWHO  = '0561109080001611692001'
001200170509     C*
001300170803     C                   Call      'UBDKC2BR'
001400170803     C                   Parm                    iDKC2BKSU
001500170803     C                   Parm                    iDKC2BTLE
001600170803     C                   Parm                    iDKC2BWHO
001700170803     C                   Parm                    oDKC2BEsito
001800170803     C                   Parm                    oDKC2BVAB
001900170803     **
002000170803     C                   eval      EDIVABDS = oDKC2BVAB
002100170803     C*
002200170803     C                   movel     vabRSD        wDiplay          52
002300170803     **
002400170803     C     wDiplay       DSPLY
002500170803     C*
002600170803     C* Imposto i valori della DS VAS
002700170803     C                   clear                   DVPC
002800170803     C                   eval      §VPCUPL = 'S'
002900170803     C                   eval      §VPCCIS = 'S'
003000170803     C                   eval      §VPCASG = 'S'
003100170803     C                   eval      §VPCSGF = 'Z'
003200170803     C                   eval      §VPCSGN = *blanks
003300170803     C                   eval      §VPCSGR = 'S'
003400170803     C                   eval      §VPCCFD = 'S'
003500170803     C                   eval      §VPCTLL = *blanks
003600170803     C                   eval      §VPCIED = 'S'
003700170803     C                   eval      §VPCTIS = 'N'
003800170803     C                   eval      §VPCSGE = 'E'
003900170803     C*
004000170803     C* Chiamata al driver di esecuzione operazioni per Personalizzazioni Clienti
004100170803     C                   eval      iUBS717VAB  =  EDIVABDS
004200170803     C                   eval      iUBS717ENV  =  'VWS'
004300170803     C                   eval      iUBS717OPZ  =  *blanks
004400170803     C                   eval      iUBS717KSU  =  iDKC2BKSU
004500170803     C                   eval      iUBS717UNI  =  DVPC
004600170803     C*
004700170803     C                   call      'UBS717R'
004800170803     C                   parm                    iUBS717VAB
004900170803     C                   parm                    iUBS717ENV
005000170803     C                   parm                    iUBS717OPZ
005100170803     C                   parm                    iUBS717KSU
005200170803     C                   parm                    iUBS717UNI
005300170803     C                   parm                    oUBS717Esito
005400170803     C                   parm                    oUBS717SI95
005500170803     C                   parm                    oUBS717DATI
005600170803     C*
005700170803     C* Se occorso errore => recepisco
005800170803     C                   if        oUBS717Esito = 0
005900170803     C                   eval      wDiplay = oUBS717DATI
006000170803     C                   endif
006100160804     **
006200160804     C     wDiplay       DSPLY
006300000000     **
006400090520     C                   Seton                                        LR
006500000000     **
