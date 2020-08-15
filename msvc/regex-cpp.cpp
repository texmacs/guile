
#include <regex>
#include <stack>
#include "libguile/gh.h"
#include "msvc/regex-cpp.h"

/* POSIX tre_regcomp() flags. */
#define REG_EXTENDED    1
#define REG_ICASE    (REG_EXTENDED << 1)
#define REG_NEWLINE    (REG_ICASE << 1)
#define REG_NOSUB    (REG_NEWLINE << 1)

/* Extra tre_regcomp() flags. */
#define REG_BASIC    0
#define REG_LITERAL    (REG_NOSUB << 1)
#define REG_RIGHT_ASSOC (REG_LITERAL << 1)
#define REG_UNGREEDY    (REG_RIGHT_ASSOC << 1)

#define REG_USEBYTES    (REG_UNGREEDY << 1)

/* POSIX tre_regexec() flags. */
#define REG_NOTBOL 1
#define REG_NOTEOL (REG_NOTBOL << 1)

/* Extra tre_regexec() flags. */
#define REG_APPROX_MATCHER     (REG_NOTEOL << 1)
#define REG_BACKTRACKING_MATCHER (REG_APPROX_MATCHER << 1)

#define REG_NOMATCH 1
typedef int regoff_t;
typedef struct {
    size_t re_nsub;  /* Number of parenthesized subexpressions. */
    void *value;       /* For internal use only. */
} regex_t;
typedef struct {
    regoff_t rm_so;
    regoff_t rm_eo;
} regmatch_t;

scm_t_bits scm_tc16_regex;

void regfree(regex_t *rx) {
    auto *e = static_cast<std::regex *>(rx->value);
    delete e;
    rx->value = NULL;
}

int regerror(int errcode, const regex_t *preg,
             char *errbuf, size_t errbuf_size) {
    printf("%s \n", __FUNCTION__);
    errbuf[0] = 'E';
    errbuf[1] = '\0';
    errbuf_size = 1;
    return 1;
}

int regcomp(regex_t *preg, const char *pattern, int cflags) {
    printf("%s %s %d\n", __FUNCTION__, pattern, cflags);
    int len = strlen(pattern);
    preg->re_nsub = 0;
    int left = 0;
    for(int i=0;i<len;i++) {
        if (pattern[i] == '(') {
            left++;
        } else if (pattern[i] == ')') {
            if (left == 0) {
                printf("%s ERROR: invalid (), %s\n", __FUNCTION__ , pattern);
            } else {
                left--;
                preg->re_nsub++;
            }
        }
    }
    if (cflags & REG_ICASE) {
        preg->value = new std::regex(pattern, std::regex::icase);
        return 0;
    }
    if (cflags & REG_NEWLINE) {
        preg->value = new std::regex(pattern, std::regex::nosubs);
        return 0;
    }
    if (cflags & REG_EXTENDED) {
        preg->value = new std::regex(pattern, std::regex::extended);
        return 0;
    }
    preg->value = new std::regex(pattern, std::regex::basic);
    return 0;
}

static size_t
regex_free(SCM obj) {
    regfree(SCM_RGX (obj));
    free(SCM_RGX (obj));
    return sizeof(regex_t);
}


SCM_SYMBOL (scm_regexp_error_key, "regular-expression-syntax");

static char *
scm_regexp_error_msg(int regerrno, regex_t *rx) {
    SCM errmsg;
    int l;

    /* FIXME: must we wrap any external calls in SCM_DEFER_INTS...SCM_ALLOW_INTS?
       Or are these only necessary when a SCM object may be left in an
       undetermined state (half-formed)?  If the latter then I believe we
       may do without the critical section code. -twp */

    /* We could simply make errmsg a char pointer, and allocate space with
       malloc.  But since we are about to pass the pointer to scm_error, which
       never returns, we would never have the opportunity to free it.  Creating
       it as a SCM object means that the system will GC it at some point. */

    errmsg = scm_make_string(SCM_MAKINUM (80), SCM_UNDEFINED);
    SCM_DEFER_INTS;
    l = regerror(regerrno, rx, SCM_STRING_CHARS (errmsg), 80);
    if (l > 80) {
        errmsg = scm_make_string(SCM_MAKINUM (l), SCM_UNDEFINED);
        regerror(regerrno, rx, SCM_STRING_CHARS (errmsg), l);
    }
    SCM_ALLOW_INTS;
    return SCM_STRING_CHARS (errmsg);
}

SCM_DEFINE (scm_regexp_p, "regexp?", 1, 0, 0,
            (SCM obj),
            "Return @code{#t} if @var{obj} is a compiled regular expression,\n"
            "or @code{#f} otherwise.")
#define FUNC_NAME s_scm_regexp_p
{
    return SCM_BOOL(SCM_RGXP(obj));
}
#undef FUNC_NAME
SCM_DEFINE (scm_make_regexp, "make-regexp", 1, 0, 1,
            (SCM pat, SCM flags),
            "Compile the regular expression described by @var{pat}, and\n"
            "return the compiled regexp structure.  If @var{pat} does not\n"
            "describe a legal regular expression, @code{make-regexp} throws\n"
            "a @code{regular-expression-syntax} error.\n"
            "\n"
            "The @var{flags} arguments change the behavior of the compiled\n"
            "regular expression.  The following flags may be supplied:\n"
            "\n"
            "@table @code\n"
            "@item regexp/icase\n"
            "Consider uppercase and lowercase letters to be the same when\n"
            "matching.\n"
            "@item regexp/newline\n"
            "If a newline appears in the target string, then permit the\n"
            "@samp{^} and @samp{$} operators to match immediately after or\n"
            "immediately before the newline, respectively.  Also, the\n"
            "@samp{.} and @samp{[^...]} operators will never match a newline\n"
            "character.  The intent of this flag is to treat the target\n"
            "string as a buffer containing many lines of text, and the\n"
            "regular expression as a pattern that may match a single one of\n"
            "those lines.\n"
            "@item regexp/basic\n"
            "Compile a basic (``obsolete'') regexp instead of the extended\n"
            "(``modern'') regexps that are the default.  Basic regexps do\n"
            "not consider @samp{|}, @samp{+} or @samp{?} to be special\n"
            "characters, and require the @samp{@{...@}} and @samp{(...)}\n"
            "metacharacters to be backslash-escaped (@pxref{Backslash\n"
            "Escapes}).  There are several other differences between basic\n"
            "and extended regular expressions, but these are the most\n"
            "significant.\n"
            "@item regexp/extended\n"
            "Compile an extended regular expression rather than a basic\n"
            "regexp.  This is the default behavior; this flag will not\n"
            "usually be needed.  If a call to @code{make-regexp} includes\n"
            "both @code{regexp/basic} and @code{regexp/extended} flags, the\n"
            "one which comes last will override the earlier one.\n"
            "@end table")
#define FUNC_NAME s_scm_make_regexp
{
    SCM flag;
    regex_t *rx;
    int status, cflags, argnum;
    SCM_VALIDATE_STRING (1, pat);
    SCM_VALIDATE_REST_ARGUMENT (flags);
    SCM_STRING_COERCE_0TERMINATION_X (pat);
/* Examine list of regexp flags.  If REG_BASIC is supplied, then
   turn off REG_EXTENDED flag (on by default). */
    cflags = REG_EXTENDED;
    flag = flags;
    argnum = 2;
    while (!SCM_NULLP (flag)) {
        int f;
        SCM_VALIDATE_INT_COPY (argnum, SCM_CAR(flag), f);
        if (f == REG_BASIC)
            cflags &= ~REG_EXTENDED;
        else
            cflags |= f;
        flag = SCM_CDR (flag);
        argnum++;
    }
    rx = SCM_MUST_MALLOC_TYPE (regex_t);
    status = regcomp(rx, SCM_STRING_CHARS (pat),
            /* Make sure they're not passing REG_NOSUB;
                       regexp-exec assumes we're getting match data.  */
                     cflags & ~REG_NOSUB);
    if (status != 0) {
        char *errmsg = scm_regexp_error_msg(status, rx);
        free(rx);
        scm_done_free(sizeof(regex_t));
        scm_error(scm_regexp_error_key,
                  FUNC_NAME,
                  errmsg,
                  SCM_BOOL_F,
                  SCM_BOOL_F);
/* never returns */
    }
    SCM_RETURN_NEWSMOB (scm_tc16_regex, rx);
}
#undef FUNC_NAME
SCM_DEFINE (scm_regexp_exec, "regexp-exec", 2, 2, 0,
            (SCM rx, SCM str, SCM start, SCM flags),
            "Match the compiled regular expression @var{rx} against\n"
            "@code{str}.  If the optional integer @var{start} argument is\n"
            "provided, begin matching from that position in the string.\n"
            "Return a match structure describing the results of the match,\n"
            "or @code{#f} if no match could be found.\n"
            "\n"
            "The @var{flags} arguments change the matching behavior.\n"
            "The following flags may be supplied:\n"
            "\n"
            "@table @code\n"
            "@item regexp/notbol\n"
            "Operator @samp{^} always fails (unless @code{regexp/newline}\n"
            "is used).  Use this when the beginning of the string should\n"
            "not be considered the beginning of a line.\n"
            "@item regexp/noteol\n"
            "Operator @samp{$} always fails (unless @code{regexp/newline}\n"
            "is used).  Use this when the end of the string should not be\n"
            "considered the end of a line.\n"
            "@end table")
#define FUNC_NAME s_scm_regexp_exec
{
    int status, nmatches, offset;
    regmatch_t *matches;
    SCM mvec = SCM_BOOL_F;

    SCM_VALIDATE_RGXP (1, rx);
    SCM_VALIDATE_STRING (2, str);
    SCM_VALIDATE_INUM_DEF_COPY (3, start, 0, offset);
    SCM_ASSERT_RANGE (3, start, offset >= 0 && offset <= SCM_STRING_LENGTH(str));
    if (SCM_UNBNDP (flags))
        flags = SCM_INUM0;
    SCM_VALIDATE_INUM (4, flags);
    SCM_STRING_COERCE_0TERMINATION_X (str);

    nmatches = SCM_RGX(rx)->re_nsub + 1;
    SCM_DEFER_INTS;
    auto *e = static_cast<std::regex *>(SCM_RGX (rx)->value);
    std::smatch sm;
    auto target = std::string(SCM_STRING_CHARS (str) + offset);
    std::regex_search(target, sm , *e);

    if (sm.empty()) {
        // TODO: ERROR handle
        /*
        scm_error(scm_regexp_error_key,
                  FUNC_NAME,
                  scm_regexp_error_msg(REG_NOMATCH, SCM_RGX (rx)),
                  SCM_BOOL_F,
                  SCM_BOOL_F);
        */
//        for (i = 0; i < nmatches; ++i) {
//            SCM_VELTS(mvec)[i + 1] = scm_cons(SCM_MAKINUM (-1), SCM_MAKINUM (-1));
//        }
    } else {
        mvec = scm_c_make_vector(nmatches + 1, SCM_UNSPECIFIED);
        SCM_VELTS(mvec)[0] = str;
        int i = 0;
        for(auto it: sm) {
            if (!it.matched) {
                SCM_VELTS(mvec)[i + 1] = scm_cons(SCM_MAKINUM (-1), SCM_MAKINUM (-1));
            } else {
                int rm_so = it.first - target.begin();
                int rm_eo = it.second - target.begin();
                SCM_VELTS(mvec)[i + 1]
                        = scm_cons(scm_long2num(rm_so + offset),
                                   scm_long2num(rm_eo + offset));
            }
            i++;
        }
    }
    SCM_ALLOW_INTS;
/* re_nsub doesn't account for the `subexpression' representing the
   whole regexp, so add 1 to nmatches. */

//    nmatches = SCM_RGX(rx)->re_nsub + 1;
//    SCM_DEFER_INTS;
//    matches = SCM_MUST_MALLOC_TYPE_NUM (regmatch_t, nmatches);
//
//    status = regexec(SCM_RGX (rx), SCM_STRING_CHARS (str) + offset,
//                     nmatches, matches,
//                     SCM_INUM (flags));
//    if (!status) {
//        int i;
//        /* The match vector must include a cell for the string that was matched,
//        so add 1. */
//        mvec = scm_c_make_vector(nmatches + 1, SCM_UNSPECIFIED);
//        SCM_VELTS(mvec)[0] = str;
//        for (i = 0; i < nmatches; ++i)
//            if (matches[i].rm_so == -1)
//                SCM_VELTS(mvec)[i + 1] = scm_cons(SCM_MAKINUM (-1), SCM_MAKINUM (-1));
//            else
//                SCM_VELTS(mvec)[i + 1]
//                        = scm_cons(scm_long2num(matches[i].rm_so + offset),
//                                   scm_long2num(matches[i].rm_eo + offset));
//    }
//    scm_must_free((char *) matches);
//    SCM_ALLOW_INTS;

//    if (status != 0 && status != REG_NOMATCH)
//        scm_error(scm_regexp_error_key,
//                  FUNC_NAME,
//                  scm_regexp_error_msg(status, SCM_RGX (rx)),
//                  SCM_BOOL_F,
//                  SCM_BOOL_F);
    return mvec;
}

#undef FUNC_NAME

void
scm_init_regex_posix() {
    scm_tc16_regex = scm_make_smob_type("regexp", sizeof(regex_t));
    scm_set_smob_free(scm_tc16_regex, regex_free);

    /* Compilation flags.  */
    scm_c_define("regexp/basic", scm_long2num(REG_BASIC));
    scm_c_define("regexp/extended", scm_long2num(REG_EXTENDED));
    scm_c_define("regexp/icase", scm_long2num(REG_ICASE));
    scm_c_define("regexp/newline", scm_long2num(REG_NEWLINE));

    /* Execution flags.  */
    scm_c_define("regexp/notbol", scm_long2num(REG_NOTBOL));
    scm_c_define("regexp/noteol", scm_long2num(REG_NOTEOL));

#include "libguile/regex-posix.x"

    scm_add_feature("regex");
}
