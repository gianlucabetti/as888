000100110623     H DFTACTGRP(*NO) ACTGRP(*NEW) BNDDIR('TRUL')
000200110623
000701161013     D pattern         S            256    varying
000702161013     D stringa         S          10000    varying
000800161012     D esito           S             10I 0
001100161013
001101161013     D wpattern        S            256    varying
001102161013     D wstringa        S          10000    varying
001103161013
001232161012     D*--------------------------------------------------------------------
001233161012     D* Compone e Verifica Regular Expression
001234161012     D*
001235161012     D*--------------------------------------------------------------------
001236161012     D regcomp         PR            10I 0 EXTPROC('regcomp')
001237161012     D   preg                          *   VALUE
001238161012     D   pattern                       *   VALUE
001239161012     D   cflags                      10I 0 VALUE
001240161012     D*
001241161012     D regexec         PR            10I 0 EXTPROC('regexec')
001242161012     D   preg                          *   VALUE
001243161012     D   string                        *   VALUE
001244161012     D   nmatch                      10U 0 VALUE
001245161012     D   pmatch                        *   VALUE
001246161012     D   eflags                      10I 0 VALUE
001247161012     D*
001248161012     D regfree         PR                  EXTPROC('regfree')
001249161012     D   preg                          *   VALUE
001250161012     D*
001251161012
001252161013     D regerror        PR            10U 0 extproc('regerror')
001253161013     D   errcode                     10I 0 value
001254161013     D   preg                          *   value
001255161013     D   errbuf                        *   value
001256161013     D   errbuf_size                 10I 0 const
001257161013
001259161012      /COPY QSYSINC/QRPGLESRC,QUSEC
001260161012
001261161012     C     *ENTRY        PLIST
001262161013     C                   PARM                    Pattern
001263161013     C                   PARM                    Stringa
001264161013     C                   Parm                    Esito
001265161013
001500161013           clear  Esito;
001501161013
001502161013             wPattern = Pattern;
001503161013             wStringa = Stringa;
001601161013
001628161013           // Se Regular Expression corrente è soddisfatta ...
001629161013               esito = Dir_RegExp(wPattern:wStringa);
001633161013
001639161012           Return;
001640161013
001641161013     P*--------------------------------------------------
001642161013     P* Procedure name: DIR_RegExp...
001643161013     P* Purpose:
001644161013     P* Returns:        Esito.
001645161013     P*--------------------------------------------------
001646161013     P DIR_RegExp...
001647161013     P                 B                   EXPORT
001648161013     D DIR_RegExp...
001649161013     D                 PI            10I 0
001650161013     D piPattern                    256    varying
001651161013     D piStringa                  10000    varying
001652161013
001653161013     D retField        S             10I 0
001654161013
001655161013     D regex_t         DS                  align
001656161013     D  re_nsub                      10I 0
001657161013     D  re_comp                        *
001658161013     D  re_cflags                    10I 0
001659161013     D  re_erroff                    10I 0
001660161013     D  re_len                       10I 0
001661161013     D  re_ucoll                     10I 0 DIM(2)
001662161013     D  re_lsub                        *   DIM(9)
001663161013     D  re_esub                        *   DIM(9)
001664161013     D  re_map                      256A
001665161013     D  re_shift                      5I 0
001666161013     D  re_dbcs                       5I 0
001667161013     D*
001668161013     D regmatch_t      DS                  occurs(2) align
001669161013     D  rm_so                        10I 0
001670161013     D  rm_ss                         5I 0
001671161013     D  rm_eo                        10I 0
001672161013     D  rm_es                         5I 0
001673161013
001674161013     D wPreg           s               *
001675161013     D wNmatch         s             10U 0 INZ(2)
001676161013     D wPmatch         s               *
001677161013     D wRv             s             10I 0
001678161013     D wPattern        s            256A
001679161013     D wStringa        s            256A
001680161013     D temp            s           1000
001681161013     D buf             S            256A
001682161014
001684161013       // Compile init
001685161013             // non pulisco perchè definite come variabili locali
001686161013                       clear regex_t;
001687161013                       clear regmatch_t;
001688161013                      %occur(regmatch_t) = 1;
001689161013                     wPmatch = %addr(regmatch_t);
001690161013                     wPreg = %addr(regex_t);
001691161013                     wPattern = %trim(piPattern) + x'00';
001692161013                     wStringa = %trim(piStringa) + x'00';
001693161013
001694161013       // Compile RE
001695161013                      wRv = regcomp(wPreg:%addr(wPattern):1);
001696161013                      if wRv <> *zeros;
001697161013                        regerror(wRv:wPreg: %addr(buf): %size(buf));
001698161013                        temp = 'regexec() failed with: ' + %str(%addr(buf));
001699161013                        return -1;
001700161013                      ENDIF;
001701161013
001702161013       // Execute RE
001703161013                  wRv = regexec(wPreg:%addr(wStringa):wNmatch:wPmatch: 0);
001704161013                      if wRv <> 0;
001705161013                        regerror(wRv:wPreg: %addr(buf): %size(buf));
001706161013                        temp = 'regexec() failed with: ' + %str(%addr(buf));
001707161013
001708161013                        return -1;
001709161013                      ENDIF;
001710161013
001711161013          // Scarico la Regular Expression corrente
001712161013               callp regfree(wPreg);
001713161013
001714161013        return 0;
001715161013
001716161013     P DIR_RegExp...
001717161013     P                 E
001718161013
001719161012
