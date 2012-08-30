from tango import db


class Setting(db.Model):
    __tablename__ = 'settings'
    id = db.Column(db.Integer, primary_key=True)
    name = db.Column(db.String(100))
    value = db.Column(db.Text())
    created_at = db.Column(db.DateTime)
    updated_at = db.Column(db.DateTime)

    def __init__(self, name, value):
        self.name = name
        self.value = value
        created_at = datetime.now()
        updated_at = datetime.now()

class Profile(db.Model):
    __tablename__ = 'profiles'
    id = db.Column(db.Integer, primary_key=True)
    uid = db.Column(db.Integer, db.ForeignKey('users.id'))
    key = db.Column(db.String(100))
    value = db.Column(db.Text())
    created_at = db.Column(db.DateTime)
    updated_at = db.Column(db.DateTime)

    def __init__(self, key, value):
        self.key = key
        self.value = value

    @classmethod
    def get(uid):
        data = {}
        profiles = Profile.query.filter_by(uid = uid).all()
        for p in profiles:
            data[p.key] = p.value
        return data

    @classmethod
    def update(uid, key, value):
        profile = Profile.query.filter_by(uid=uid, key=key).first()
        if profile:
            profile.value = value
        else:
            profile = Profile(uid, key, value)
            db.session.add(profile)

    @classmethod
    def find(key, profiles):
        for p in profiles:
            if key == p.key:
                return p.value
        return None 

class Query(db.Model):
    __tablename__ = 'queries'
    id = db.Column(db.Integer, primary_key=True)
    uid = db.Column(db.Integer, db.ForeignKey('users.id'))
    tab = db.Column(db.String(100))
    mod = db.Column(db.String(100))
    name = db.Column(db.String(200))
    filters = db.Column(db.String)
    is_public = db.Column(db.Boolean)
    created_at = db.Column(db.DateTime)
    updated_at = db.Column(db.DateTime) 

