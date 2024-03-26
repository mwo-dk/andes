namespace Andes.Services;

public static class BuildService
{
    public static bool IsDebug() =>
#if DEBUG
        true;
#else
        false;
#endif
}
