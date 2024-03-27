using System.Runtime.Serialization;

namespace Andes.Shared;
public enum EquationNumberingTag
{
    [EnumMember(Value = "none")]
    None,
    [EnumMember(Value = "ams")]
    Ams,
    [EnumMember(Value = "all")]
    All
}